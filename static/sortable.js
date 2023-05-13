document.addEventListener('ihp:load', () => {

    // Init sortable.
    document.querySelectorAll('.js-sortable').forEach(function (elem) {
        if (Boolean(elem.jsSortableInitialized) === false) {
            Sortable.create(elem, {
                handle: '.sortable-handle',
                animation: 150,
            });
            elem.jsSortableInitialized = true;
        }
    });
});