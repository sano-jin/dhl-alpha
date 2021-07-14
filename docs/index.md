# dhl-alpha

[_ミニマルなグラフ書き換え言語のコンパイラ及びランタイム_](https://github.com/sano-jin/dhl-alpha)

[![logo](./DHL-logo-alpha.svg)](https://github.com/sano-jin/dhl-alpha)


## 概要
ポインタベースの有向ハイパーグラフを，ガーベージコレクションを用いなくとも，
メモリ安全かつメモリリークを起こさずに，第一級に扱うことのできる言語を設計・実装した．
コンパイラとランタイムを両方合わせても 800 LOC 程度に抑えた

> ランタイムも OCaml を用いているので，結局 GC を使ってしまってはいるが，
> アトムを「削除」する際はわざわざアトム名にゴミを入れて，それにアクセスしていないことを確認することで，
> 「メモリ安全」であることを確かめている


## プログラム構成

全部で 822 LOC

[ocamldoc により生成したドキュメント](https://sano-jin.github.io/dhl-alpha/ocamldoc/dhl/index.html)


### Compiler

- parse: 105 LOC
    - 字句解析・構文解析を行う
- analyzer: 139 LOC
    - 意味解析を行う
- generator: 182 LOC
    - 中間命令列を生成する

### VM
- vm: 309 LOC
    - 仮想マシン

### Utility
- util: 87 LOC
    - 共用モジュール




