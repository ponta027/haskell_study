# mqtt-sample


## 設定手順

stack.yaml のresolverとextra-depsを修正する。

```
resolver: lts-11.22
#resolver: lts-12.26
```


```
extra-deps:
    - mqtt-hs-1.0.2
    - ghc-boot-th-8.4.1
    - base-4.11.1.0
    - network-2.6.3.6
    - stm-2.4.5.1
``` 



