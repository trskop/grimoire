(function() {
    const anchor = function(id) {
        return `<a href="#${id}" class="anchor-link">🔗</a>`;
    };

    const elements = Array.apply(null, document.querySelectorAll('h1,h2,h3,h4,h5,h6'))
    elements.forEach(elm => {
        if (elm.id !== '') {
            elm.insertAdjacentHTML('beforeend', anchor(elm.id));
        }
    });
})();
