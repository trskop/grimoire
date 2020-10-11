// Based on https://github.com/jperasmus/docsify-copy-code
(function() {
    const template = [
        '<button class="copy-code-button">',
        '<span class="label">Copy to clipboard</span>',
        '<span class="error">Error</span>',
        '<span class="success">Copied</span>',
        '</button>'
    ].join('');

    Array.apply(null, document.querySelectorAll('pre.code-block')).forEach(elm => {
        elm.insertAdjacentHTML('beforeend', template);
    });

    document.querySelector('article').addEventListener('click', function(evt) {
        const isCopyCodeButton = evt.target.classList.contains('copy-code-button');

        if (isCopyCodeButton) {
            const buttonElm = evt.target.tagName === 'BUTTON' ? evt.target : evt.target.parentNode;
            const range = document.createRange();
            const preElm = buttonElm.parentNode;
            const codeElm = preElm.querySelector('code');

            let selection = window.getSelection();

            range.selectNode(codeElm);
            selection.removeAllRanges();
            selection.addRange(range);

            try {
                const successful = document.execCommand('copy');

                if (successful) {
                    buttonElm.classList.add('success');
                    setTimeout(function() {
                        buttonElm.classList.remove('success');
                    }, 1000);
                }
            }
            catch(err) {
                console.error(`copy-code: ${err}`);

                buttonElm.classList.add('error');
                setTimeout(function() {
                    buttonElm.classList.remove('error');
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
