# redis-sample


## Command

```
stack exec redis-sample-exe publish
```

foo ,bar ,baz:1,baz:2 に対してpublish する。



```
stack exec redis-sample-exe subscribe
```

foo2, baz に対してsubscribeする。



```
stack exec redis-sample-exe client
```

key:"hello" に"hello"を書き込む。
key:"world" に、"world"を書き込む。

それぞれのKeyを取得して、表示する。



