# haskell_study


## chat-server

## mqtt-sample

* sample code for mqtt-hs

## multicores

## redis-sample

* sample code for hedis

## spock-sample

* spock sample 

## xml-parse-sample

* parse xml 



## rasberrypiでstackを動作させる。



https://docs.haskellstack.org/en/stable/README/



### stackのインストール

```
> curl -sSL https://get.haskellstack.org/ | sh
> stack new sample
> stack build
```

エラー発生

```
No setup information found for ghc-8.6.5 on your platform.
This probably means a GHC bindist has not yet been added for OS key 'linux-armv7'.
Supported versions: ghc-7.10.2, ghc-7.10.3, ghc-8.0.1, ghc-8.0.2, ghc-8.2.1, ghc-8.2.2, ghc-8.6.3

```

ghc-8.6.3に変更して、再度ビルド
再度エラー


armの命令が不足しているとエラーが出力される。

```
[1 of 2] Compiling Main             ( /home/pi/.stack/setup-exe-src/setup-mPHDZzAJ.hs, /home/pi/.stack/setup-exe-src/setup-mPHDZzAJ.o )
/tmp/ghc20362_0/ghc_6.s: Assembler messages:

/tmp/ghc20362_0/ghc_6.s:43:0: error:
     Error: selected processor does not support `movw r7,:lower16:stg_bh_upd_frame_info' in ARM mode
   |
43 |         movw    r7, :lower16:stg_bh_upd_frame_info

```

stack ghc でコンパイルしてみるが、同様のエラーが出る。
コンパイルオプションでアーキテクチャ指定していないためエラーが出ているっぽい。
アーキテクチャ指定してビルドする。 

```
ghc -opta-march=armv7-a Main.hs
```

ビルド成功。
stack buildでも同様にコンパイルオプションでアーキテクチャを指定できるようにする。
$HOME/.stack/config.yaml
あたりをいじってみたが。。。いまいち反映されない。
 ~/.stack/programs/arm-linux/ghc-8.6.3/bin/ghc-8.6.3
のスクリプトを修正する。

```
cat ~/.stack/programs/arm-linux/ghc-8.6.3/bin/ghc-8.6.3

#!/bin/sh
exedir="/home/pi/.stack/programs/arm-linux/ghc-8.6.3/lib/ghc-8.6.3/bin"
exeprog="ghc-stage2"
executablename="$exedir/$exeprog"
datadir="/home/pi/.stack/programs/arm-linux/ghc-8.6.3/share"
bindir="/home/pi/.stack/programs/arm-linux/ghc-8.6.3/bin"
topdir="/home/pi/.stack/programs/arm-linux/ghc-8.6.3/lib/ghc-8.6.3"
executablename="$exedir/ghc"
exec "$executablename"  -opta-march=armv7-a -B"$topdir" ${1+"$@"}
#exec "$executablename" -B"$topdir" ${1+"$@"}
```

この変更でビルドしたら成功した。

多分本来はglobalの設定で反映できるのだろうが、今のところこれで運用する。










