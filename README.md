# My Emacs Config

需要先安装 [Cask](https://github.com/cask/cask)

```sh
(cd ~ && git clone https://github.com/bolasblack/.emacsrc.git && echo '(load "~/.emacsrc/init.el")' >> .emacs && cask --path ~/.emacsrc install)
```

* `conf/` 是主要的配置文件所在地
* `snippets/` snippet 文件所在地
* `macro-lisp.el` 里的是一些零碎的代码
