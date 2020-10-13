(function() {
    const anchor = function(id) {
        return `<a href="#${id}" class="anchor-link">🔗</a>`;
    };

    Array.apply(null, document.querySelectorAll('h1,h2,h3,h4,h5,h6')).forEach(elem => {
        if (elem.id !== '') {
            elem.insertAdjacentHTML('beforeend', anchor(elem.id));
        }
    });
})();
