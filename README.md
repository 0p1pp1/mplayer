# ISDB-T/SのDVBデバイスからの再生や、保存したTSファイル再生のためのパッチ #

## 変更履歴 ##
+ ver 1.3.1
  - mplayerの元ソース更新・マージ(rev37551), 最新ffmpeg(commit:a3304302)のAPIへ対応

+ ver 1.3
  - vaapiのサポート追加 (mplayer-vaapiから)

+ ver 1.2
  - デスクランブル機能の切り出し、外部ライブラリ(libdemulti2)への分離
  - 番組切り替わり時に映像/音声が乱れていた問題を軽減
  - VDPAUで再生していると、映像サイズやアスペクトの切り替わり時にエラーが発生する問題を修正

+ ver 1.10
  - 字幕再生機能の追加

  - 1TSに複数局の番組が含まれている場合における
    ビデオ/音声/字幕ストリームの自動選択や切り替えを改良

  - 番組切り替わりや再生開始の時に、SEGVで落ちる場合があるのを修正

  - その他メモリリークやクラッシュの修正

  - 最新のmplayerソースにrebase, パッチの機能別分類廃止, ビルド手順整理

## パッチの内容(全体) ##

- S2API(DVBv5)対応:  
   .mplayer/channels.conf[.s2] にS2API用パラメータを記載可能なフォーマットを追加。  
   BS/CS (ISDB-S)は旧API(DVBv3)ではアクセスできないため、S2APIの利用が必須となる。  
   channels.confファイルは、dvbアプリ集のs2scanコマンドで生成可能

- DVBデバイスへのアクセス・パラメータ指定の改良

  * アクセスできるDVBデバイスの最大数を4->16へ増加  
  PT1/2/3複数枚挿しやFriio等他DVBデバイスとの併用に対応。
  ただし使用できるDVBデバイスの最大数は、カーネルのconfigオプションの
  CONFIG\_DVB\_MAX\_ADAPTERS (デフォルト:8)によっても制限されていることに注意。

  * 地上波用とBS/CS110用のチャンネル設定ファイルの統合  
  channels.conf[.s2]に地デジ用とBS/CS110用定義をまとめて記載できるようにし,
  DVBデバイスのタイプに応じて使用できる行(=チャンネル定義)を自動選別できるように修正。  
    ```
    ex.  
      NHK総合:DTV_DELIVERY_SYSTEM=8|DTV_FREQUENCY=521142857:3088  
      NHKBSHi:DTV_DELIVERY_SYSTEM=9|DTV_FREQUENCY=1318000|DTV_ISDBS_TS_ID=0x40f2:103  
        ---> mplayer dvb://1@NHKBSHi
    ```

  * 使用するDVBデバイスの番号指定を変更  
   カード番号が1からスタートしていたのを0スタートとし、
   /dev/dvb/adapterN のNと一致するように変更 (ex. `mplayer dvb://0@NHK総合`)

- 映像・音声の再生力の強化
  * suspend/resumeへの対応

  * 画面サイズ切り替わり(SD <-> HD)への対応

  * 音声切り替わりへの対応 (ffmpeg本家での対応の不足分)

  * dual monoへの対応/切り替え (-dualmono オプション追加)  
   音声がdual monoの場合 -dualmono 0 でmain/leftチャネル、1でsub/right, 2で両方を再生。  
   コマンドでの切り替えも対応。 詳しくはreadme-dmono.txtを参照

  * BS降雨時対応版の選択 (-lowcnオプション追加)
    + -lowcn無しの場合: 降雨時版の映像・音声は選択されなくなる
    + -lowcnを付けた場合: 降雨時版を優先的に選択

  * 番組・音声の切り替え機能の強化  
   同一TS内に複数のプログラム、各プログラムに複数の音声・映像を持つ場合における、
   映像・音声ストリームの自動選択や切り替え方法の改良  
   以下の優先順位で選択される。

        1. ユーザが指定した-aid/-vid/-sid
        2. -alangで指定された言語と一致する音声PES
        3. 映像・音声・字幕それぞれのデフォルトPES
        4. 先に見つかったPES

    降雨時用PESや、異なるプログラムに属する映像・音声の組み合わせは除外して切り替える。  
    dual monoの各チャンネルは、それぞれ別のストリームのように切り替え・選択される。

  * その他映像・音声再生バグの修正

- 字幕の再生(実験的サポート)  
  `mplayer -ass ...` で再生中にsub_demuxコマンド('j'キー)で字幕表示が切り替わる。
  詳しくはreadme-sub.txtを参照

- 外部ライブラリlibdemulti2によるMULTI2復号化  
   dvb://だけでなくTSファイルの再生時にも利用される。  
  (libdemulti2は配布していない。 APIについてはヘッダファイルdemulti2.hを参照)

- VA API対応のマージ  
  `例: mplayer -va vaapi -vo vaapi:colorspace=2 foo.ts`

  復号機能は `-va vaapi` で、表示機能は `-vo vaapi`で指定する。  
  `mplayer -vo vaapi:xxx "some existing TS-file"`のように、
  無効なオプションをつけて実行するとヘルプが出力される。

  複合ではffmpegを使うので、下記手順5.でvaapiがenableされるようにすること。 

  注意： マルチスレッドで復号すると(`-lavdopts threads=4` とか)絵が壊れたりクラッシュする。


## ビルド手順の例 ##
本レポジトリからパッチ適用済ソース("isdb"ブランチ)を入手してビルドする。
mplayerのビルドでは、依存するffmpegライブラリについて、
共有ライブラリを動的リンクするか、
ソースディレクトリ下にffmpegソースのコピーを用意し静的リンクするか選択できるが、
ここでは(デフォルトかつ推奨の方法である)後者の方法でビルドする。

1. 必要なパッケージのインストール
 - デスクランブルを行う場合  
   libdemulti2 と libyakisoba{,-devel} か libsobacas{,-devel} か pcsc-lite{,-devel}

 - 字幕機能を使う場合  
   fontconfig{,-devel} と harfbuzz{,-devel} と freetype{,-devel}

2. ソースの準備(mplayer)

 a) パッチ済ソースを展開

    ```
    git clone [--depth 1] https://github.com/0p1pp1/mplayer.git
    ```

 あるいは  
 b) 既に以前に上記a) git clone ... を実行済で新規の差分だけ入手する場合

    ```
    cd mplayer
    git pull origin isdb
    ```

 あるいは  
 c) 既にmplayer本家のレポジトリのcloneがある場合 (未確認)

    ```
    cd mplayer
    git remote add isdb https://github.com/0p1pp1/mplayer.git
    git pull isdb isdb
    ```

3. ソースの準備(ffmpeg) : 最新版のffmpegを使いたい場合は省略可。  

 a) リリース版のソースを使う場合

    ```
    cd mplayer
    wget http://ffmpeg.org/releases/ffmpeg-2.8.2.tar.bz2
    tar xjvf ffmpeg-2.8.2.tar.bz2
    mv ffmpeg-2.8.2 ffmpeg
    ```

 b) ffmpegのgitレポジトリからcloneする場合

    ```
    git clone [--depth 1] git://source.ffmpeg.org/ffmpeg.git
    ```

 c) 既に上記b)等でcloneした(古い)レポジトリを更新する場合

    ```
    cd mplayer/ffmpeg
    git pull [--unshallow] # --unshallowは、動作確認済みリビジョンを使いたい場合のみ必要
    [ git checkout a3304302  # 動作確認済みリビジョンを使いたい場合のみ ]
    cd ..
    ```

4. mplayerのconfigure

    ```
    ./configure --confdir=/etc --disable-vidix --disable-gif --disable-directfb \
                --enable-ass-internal --enable-menu --enable-vaapi \
                --extra-cflags=-I/usr/local/include --extra-ldflags=-L/usr/local/lib
    ```

  上の手順3.を省いていると、
  configureの途中でffmpegのソースを入手するか聞いてくるので、ENTER で了承する。
  その場で自動的にgit clone --depth 1 ... でffmpegのサイトから最新版がダウンロードされる。

  デスクランブル関連の機能については、libdemulti2がインストールされているか検出され、
  利用可能であれば自動的に使用するよう設定されている。
  ヘッダファイルやライブラリが標準の場所にインストールされていなければ、
  適宜CFLAGSやLDFLAGSなどを設定して./configureを実行する必要がある。  
  `./configure --extra-cflags=... --extra-ldflags=...` でも指定できる。

   - (強制的に)デスクランブル機能を組み込まない場合: `--disable-bcas`を追加

   - 字幕機能が要らない場合: 上記`--enable-ass-internal`は不要

   - VA API機能が不要な場合: 上記`--enable-vaapi`は不要。

5. ffmpegのconfigure,ビルド

    ```
    cd ffmpeg
    ./configure --enable-gpl --enable-nonfree --enable-libopus --enable-pic \
                --disable-doc --disable-lzma --disable-programs
    cd ..
    make ffmpeglibs
    ```

6. ビルド

    ```
    make
    sudo make install # 必要に応じて。インストールしなくても直接実行できる。
    ```

  ffmpegの最新版ソースが原因でmplayerのビルドでエラーが出る場合は、手順3(c)からやり直し


7. テスト・設定

  `(例) ./mplayer ~/foo.ts`

  必要に応じて以下の設定ファイルを~/.mplayer/に作成し、カスタマイズする。
  - config: (etc/example.conf にサンプル)
  - input.conf, menu.conf等: etc/に同名のサンプル

  DVBの再生を利用するなら
  - channels.conf: (s2scanスクリプト等で)

### configファイルの例 ###
(./DOCS/man/en/mplayer.1を参照)

```
[default]
ao=pulse,
vo=vaapi:dm=1:colorspace=0,xv,
va=vaapi
stop-xscreensaver=yes
joystick=no
ar=no
lirc=no
volstep=10
mc=0.1
subfont-osd-scale=5
subfont-autoscale=1
ass=yes
font=/usr/share/fonts/dejavu/DejaVuSans.ttf

[protocol.dvb]
profile-desc="profile for dvb:// streams"
capture=yes
cache=4096
cache-min=50
dvbin=timeout=200
framedrop=1
utf8=yes
```
