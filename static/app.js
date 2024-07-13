$(document).on('ihp:unload', () => {
    // Unload bindings.
    $('form:not([data-disable-form-submit-observe-change])').each(function () {
        this.initDisableSubmitButtonIfFormNotChanged = false;
    });

    $('.js-sortable').each(function () {
        this.initSortable = false;
    });
});


$(document).on('ihp:load', () => {
    initDisableSubmitButtonIfFormNotChanged();
    initSortable();
});

const initDisableSubmitButtonIfFormNotChanged = function () {
    // Disable submit button if form has not changed.
    $('form[data-disable-form-submit-observe-change]').each(function () {
        // Mark submit button as enabled.
        $(this).find('button[type=submit]')
            .toggleClass('disabled', false)
            .toggleClass('enabled', true);
    });

    $('form:not([data-disable-form-submit-observe-change])').each(function () {
        if (Boolean(this.initDisableSubmitButtonIfFormNotChanged) === true) {
            // Already binded.
            return;
        }
        this.initDisableSubmitButtonIfFormNotChanged = true;
        const $this = $(this);

        const formData = new FormData(this);

        // Helper function to convert a value to a string representation.
        // File objects are converted to a string like "FILE:filename-size-type".
        function serializeValue(value) {
            if (value instanceof File && value.name) {
                return `FILE:${value.name}-${value.size}-${value.type}`;
            }
            return value;
        }

        // Helper function to convert form data to an array of key-value pairs
        function formDataToArray(formData) {
            const formDataEntries = [];
            for (let pair of formData.entries()) {
                const key = pair[0];
                const value = pair[1];
                formDataEntries.push({ [key]: serializeValue(value) });
            }
            return formDataEntries;
        }

        const formDataEntries = formDataToArray(formData);
        $this.data('formDataSerialized', JSON.stringify(formDataEntries));
        $this.find('button[type=submit]').prop('disabled', true);

        $this.on('change input', function () {
            const $this = $(this);
            const formData = new FormData(this);
            const formDataEntries = formDataToArray(formData);
            const changed = $this.data('formDataSerialized') !== JSON.stringify(formDataEntries);

            $this.find('button[type=submit]')
                .prop('disabled', !changed)
                .toggleClass('disabled', !changed)
                .toggleClass('enabled', changed);

            $this.find('.form-status-wrapper').remove();
        });
    });

    // Show the remove file checkbox if a file is uploaded.
    $('form .file-upload-wrapper :file').on('change', function () {
        const $this = $(this);

        const $wrapper = $this.closest('.file-upload-wrapper');
        $wrapper
            .find('.remove-file-wrapper')
            .toggleClass('hidden', !$this.val());

        if (!!$this.val()) {
            // A file is uploaded, uncheck the "Remove file" checkbox,
            // in case it was previously checked.
            $wrapper
                .find(':checkbox')
                .prop('checked', false);
        }
    });

    // Hide the file upload wrapper if the remove file checkbox is checked.
    $('form .remove-file-checkbox:checkbox').on('change', function () {
        const $wrapper = $(this).closest('.file-upload-wrapper');
        $wrapper
            .find(':file')
            .val(null);

        $wrapper
            .find('img')
            // We don't null the src, so we we don't get a broken image icon.
            // from https://stackoverflow.com/a/9967193/750039
            .attr('src', 'data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==');

        $wrapper
            .find('a.download-link')
            .hide();

        // Hide the remove file checkbox.
        $wrapper.find('.remove-file-wrapper')
            .addClass('hidden');

    });
}


const initSortable = function () {
    // Init sortable.
    $('.js-sortable').each(function () {
        if (Boolean(this.initSortable) === true) {
            // Already binded.
            return;
        }

        this.initSortable = true;
        Sortable.create(this, {
            handle: '.sortable-handle',
            animation: 150,
        });
    });
}