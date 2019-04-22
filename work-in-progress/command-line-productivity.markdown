---
title: Command Line Productivity Incantations
---

* [`fzf`](https://github.com/junegunn/fzf)
* [`fzy`](https://github.com/jhawthorn/fzy)
* [ripgrep `rg`](https://github.com/BurntSushi/ripgrep)
* [`bfs`](https://github.com/tavianator/bfs)
* [`bat`](https://github.com/sharkdp/bat)
* `jq`
* `direnv`
* `kitty`

Using `jq` as a templating language for JSON:

```
jq  --argjson name '"John Snow"' \
    --argjson email '"knows.nothing@winterfell.westeros"' \
    --null-input '{name: $name, email: $email}'
```
