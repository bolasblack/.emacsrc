# My Emacs Config

```sh
cd ~ && git clone https://github.com/bolasblack/.emacsrc.git && echo '(load "~/.emacsrc/early-init.el")' > ~/.emacs.d/early-init.el && echo '(load "~/.emacsrc/init.el")' >> ~/.emacs.d/init.el
```

## 目录结构

* `early-init.el` 启动优化配置，需要被 `~/.emacs.d/early-init.el` 加载
* `init.el` 主入口，需要被 `~/.emacs.d/init.el` 加载
* `conf/` 主要的配置文件所在地
* `lib/` 一些自己写的脚本
* `snippets/` snippet 文件所在地
* `flycheck.conf/` flycheck 的配置文件所在地
