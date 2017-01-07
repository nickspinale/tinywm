# TinyWM

TinyWM is a tiny window manager written in Haskell, inspired by [the original TinyWM](http://incise.org/tinywm.html).

It exists mostly as a simple test case for the following libraries I've written for [XHB](https://hackage.haskell.org/package/xhb):

- [xhb-requests](https://github.com/nickspinale/xhb-requests)
- [xhb-monad](https://github.com/nickspinale/xhb-monad)
- [xhb-keysyms](https://github.com/nickspinale/xhb-keysyms)
- [xhb-mapping-state](https://github.com/nickspinale/xhb-mapping-state)

### Usage

- Focus follows pointer.
- Alt+Button1, drag: interactive window move
- Alt+Button3, drag: interactive window resize
- Alt+F1: raise focused window
