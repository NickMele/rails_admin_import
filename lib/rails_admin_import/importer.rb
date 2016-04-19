require "rails_admin_import/import_logger"

module RailsAdminImport
  class Importer
    extend ActiveModel::Callbacks

    define_model_callbacks :import, only: :around

    around_import :around_import_callback

    def initialize(import_model, params)
      @import_model = import_model
      @params = params
      @results = { success: [], error: [], skipped: [] }
    end

    attr_reader :import_model, :params, :results

    class UpdateLookupError < StandardError; end

    def import(records)
      run_callbacks :import do
        begin
          if records.count > RailsAdminImport.config.line_item_limit
            return results = {
              success: [],
              error: [I18n.t('admin.import.import_error.line_item_limit', limit: RailsAdminImport.config.line_item_limit)]
            }
          end

          with_transaction do
            records.each do |record|
              import_record(record)
            end

            rollback_if_error
          end
        rescue Exception => e
          report_general_error("#{e} (#{e.backtrace.first})")
        end
      end

      format_results
    end

    private

    def around_import_callback
      perform_callback(import_model.model, :before_import)

      if import_model.model.respond_to? :around_import
        perform_callback_with_yield(import_model.model, :around_import) { yield }
      else
        yield
      end

      perform_callback(import_model.model, :after_import)
    end

    def with_transaction(&block)
      if RailsAdminImport.config.rollback_on_error &&
        defined?(ActiveRecord)

        ActiveRecord::Base.transaction &block
      else
        block.call
      end
    end

    def rollback_if_error
      if RailsAdminImport.config.rollback_on_error &&
        defined?(ActiveRecord) &&
        !results[:error].empty?

        results[:success] = []
        raise ActiveRecord::Rollback
      end
    end

    def import_record(record)
      if !update_lookup.nil? && !record.has_key?(update_lookup)
        raise UpdateLookupError, I18n.t("admin.import.missing_update_lookup")
      end

      object = if import_model.model.respond_to? :import_find_or_create
                 perform_callback(import_model.model, :import_find_or_create, record)
               else
                 find_or_create_object(record, update_lookup)
               end

      if !object.new_record? && skip_if_exists?
        report_skipped(object)
        return
      end

      object.attributes = object_attributes(record)

      action = object.new_record? ? :create : :update

      begin
        import_single_association_data(object, record)
        import_many_association_data(object, record)
      rescue AssociationNotFound => e
        error = I18n.t("admin.import.association_not_found", :error => e.to_s)
        report_error(object, action, error)
        return
      end

      perform_callback(object, :before_import_save, record)

      if object.save
        report_success(object, action)
        perform_callback(object, :after_import_save, record)
      else
        report_error(object, action, object.errors.full_messages.join(", "))
      end
    end

    def skip_if_exists?
      @skip_if_exists ||= params[:skip_if_exists] == "1"
    end

    def update_if_exists?
      @update_if_exists ||= params[:update_if_exists] == "1"
    end

    def update_lookup
      @update_lookup ||= params[:update_lookup].try(:to_sym) if update_if_exists? || skip_if_exists?
    end

    attr_reader :results

    def logger
      @logger ||= ImportLogger.new
    end

    def report_success(object, action)
      object_label = import_model.label_for_model(object)
      message = I18n.t("admin.import.import_success.#{action}",
                       :name => object_label)
      logger.info "#{Time.now}: #{message}"
      results[:success] << message
    end

    def report_error(object, action, error)
      object_label = import_model.label_for_model(object)
      message = I18n.t("admin.import.import_error.#{action}",
                       :name => object_label,
                       :error => error)
      logger.info "#{Time.now}: #{message}"
      results[:error] << message
    end

    def report_skipped(object)
      object_label = import_model.label_for_model(object)
      message = I18n.t("admin.import.import_skipped", :name => object_label)
      logger.info "#{Time.now}: #{message}"
      results[:skipped] << message
    end

    def report_general_error(error)
      message = I18n.t("admin.import.import_error.general", :error => error)
      logger.info "#{Time.now}: #{message}"
      results[:error] << message
    end

    def format_results
      imported = results[:success]
      not_imported = results[:error]
      skipped = results[:skipped]
      unless imported.empty?
        results[:success_message] = format_result_message("successful", imported)
      end
      unless not_imported.empty?
        results[:error_message] = format_result_message("error", not_imported)
      end
      unless skipped.empty?
        results[:skipped_message] = format_result_message("noaction", skipped)
      end

      results
    end

    def format_result_message(type, array)
      result_count = "#{array.size} #{import_model.display_name.pluralize(array.size)}"
      I18n.t("admin.flash.#{type}",
             name: result_count,
               action: I18n.t("admin.actions.import.done"))
    end

    def perform_callback(object, method_name, record = nil)
      if object.respond_to? method_name
        # Compatibility: Old import hook took 2 arguments.
        # Warn and call with a blank hash as 2nd argument.
        if object.method(method_name).arity == 2
          report_old_import_hook(method_name)
          object.send(method_name, record, {})
        else
          object.send(method_name) if record.nil?
          object.send(method_name, record) unless record.nil?
        end
      end
    end

    def perform_callback_with_yield(object, method_name)
      object.send(method_name) { yield } if object.respond_to? method_name
    end

    def report_old_import_hook(method_name)
      unless @old_import_hook_reported
        error = I18n.t("admin.import.import_error.old_import_hook",
                       model: import_model.display_name,
                       method: method_name)
        report_general_error(error)
        @old_import_hook_reported = true
      end
    end

    def find_or_create_object(record, update)
      model = import_model.model
      object = if update.present?
                 model.where(update => record[update]).first
               end

      return model.new if object.nil?
      return object
    end

    def object_attributes(record)
      field_names = import_model.model_fields.map(&:name)
      new_attrs = record.select do |field_name, value|
        field_names.include?(field_name) && !value.blank?
      end

      return new_attrs
    end

    def import_single_association_data(object, record)
      import_model.single_association_fields.each do |field|
        mapping_key = params[:associations][field.name]
        value = extract_mapping(record[field.name], mapping_key)

        if !value.blank?
          object.send "#{field.name}=", import_model.associated_object(field, mapping_key, value)
        end
      end
    end

    def import_many_association_data(object, record)
      import_model.many_association_fields.each do |field|
        if record.has_key? field.name
          mapping_key = params[:associations][field.name]
          values = record[field.name].reject { |value| value.blank? }.map { |value|
            extract_mapping(value, mapping_key)
          }

          if !values.empty?
            associated = values.map { |value| import_model.associated_object(field, mapping_key, value) }
            object.send "#{field.name}=", associated
          end
        end
      end
    end

    def extract_mapping(value, mapping_key)
      if value.is_a? Hash
        value[mapping_key]
      else
        value
      end
    end
  end
end
