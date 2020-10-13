// Based on https://github.com/jperasmus/docsify-copy-code
(function() {
    const template = [
        '<button class="copy-code-button">',
        '<span class="label">Copy to clipboard</span>',
        '<span class="error">Error</span>',
        '<span class="success">Copied</span>',
        '</button>'
    ].join('');

    Array.apply(null, document.querySelectorAll('pre.code-block')).forEach(elem => {
        elem.insertAdjacentHTML('beforeend', template);
    });

    // When Pandoc does syntax highlighting for a code block we end up:
    //
    // <div>
    //   <pre><code>...</code></pre>
    // </div>
    //
    // Any custom attributes of the key=value kind will end up being attached
    // to <div> instead of <pre>. If there is no highlighting happening then it
    // will end up being attached to <pre>.
    //
    // We use the 'data-lang' attribute to pass the name of the language inside
    // the code block so that it can be displayed.
    Array.apply(null, document.querySelectorAll('div[data-lang] > pre.code-block')).forEach(elem => {
        elem.dataset.lang = elem.parentNode.dataset.lang;
    });

    document.querySelector('article').addEventListener('click', function(evt) {
        const isCopyCodeButton = evt.target.classList.contains('copy-code-button');

        if (isCopyCodeButton) {
            const buttonElem = evt.target.tagName === 'BUTTON' ? evt.target : evt.target.parentNode;
            const range = document.createRange();
            const preElem = buttonElem.parentNode;
            const codeElem = preElem.querySelector('code');

            let selection = window.getSelection();

            range.selectNode(codeElem);
            selection.removeAllRanges();
            selection.addRange(range);

            try {
                const successful = document.execCommand('copy');

                if (successful) {
                    buttonElem.classList.add('success');
                    setTimeout(function() {
                        buttonElem.classList.remove('success');
                    }, 1000);
                }
            }
            catch(err) {
                console.error(`copy-code: ${err}`);

                buttonElem.classList.add('error');
                setTimeout(function() {
                    buttonElem.classList.remove('error');
                }, 1000);
            }

            selection = window.getSelection();

            if (typeof selection.removeRange === 'function') {
                selection.removeRange(range);
            }
            else if (typeof selection.removeAllRanges === 'function') {
                selection.removeAllRanges();
            }
        }
    });
})();
