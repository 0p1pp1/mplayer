# ISDB-T/SのDVBデバイスからの再生や 保存したTSファイル再生のためのパッチ #

## 変更履歴 ##
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

- S2API対応: .mplayer/channels.conf[.s2] にS2API用パラメータを記載可に
   BS/CS (ISDB-S)は旧APIではアクセスできないため。
   dvbアプリ集のs2scanコマンドで生成可能

- DVBデバイスへのアクセス・パラメータ指定の改良
 * アクセスできるDVBデバイスの最大数を4->16へ増加
    PT1/2複数枚or他デバイスとの併用に対応.

    ただし使用できるDVBデバイスの最大数は、カーネルのconfigオプションの
    CONFIG_DVB_MAX_ADAPTERS (デフォルト:8?)によっても制限されていることに注意。
 * channels.conf[.s2]に地デジ用とBS/CS110用定義をまとめて記載できるようにし,
    DVBデバイスのタイプに応じて使用できるチャンネル定義を選別できるように修正.

        ex. NHK総合:DTV_DELIVERY_SYSTEM=8|DTV_FREQUENCY=521142857:3088
            NHKBSHi:DTV_DELIVERY_SYSTEM=9|DTV_FREQUENCY=1318000|DTV_ISDBS_TS_ID=0x40f2:103
        ---> mplayer dvb://1@NHKBSHi

 * DVBデバイス(カード)の番号が1からスターとしていたのを0スタートとして
   /dev/dvb/adapterN のNと一致するように変更 (ex. `mplayer dvb://0@NHK総合`)

- 映像・音声の再生力の強化
 * suspend/resumeへの対応

 * 画面サイズ切り替わり(SD <-> HD)への対応

 * 音声切り替わりへの対応 (ffmpeg本家での対応の不足分)

 * dual monoへの対応/切り替え (-dualmono オプション追加)
     音声がdual monoの場合 -dualmono 0 でmain/leftチャネル、1でsub/right, 2で両方を再生
     コマンドでの切り替えも対応  詳しくはreadme-dmono.txtを参照

 * BS降雨時対応版の選択 (-lowcnオプション追加)
    + -lowcn無しの場合: 降雨時版の映像・音声は選択されなくなる
    + -lowcnを付けた場合: 降雨時版を優先的に選択

 * 番組・音声の切り替え機能の強化
     同一TS内に複数のプログラム、各プログラムに複数の音声・映像を持つ場合における、
     映像・音声ストリームの自動選択や切り替え方法の改良

     1. ユーザが指定した-aid/-vid/-sidb
     2. -alangで指定された言語と一致する音声PES
     3. 映像・音声・字幕それぞれのデフォルトPES
     4. 先に見つかったPES
      の順で優先的に選択する。

      降雨時用PESや、異なるプログラムに属する映像・音声の組み合わせは除外して切り替え
      dual monoの各チャンネルをそれぞれ別のストリームのように切り替え・選択

 * その他映像・音声再生バグの修正

- 字幕の再生(実験的サポート)
  mplayer -ass ... で再生中にsub_demuxコマンド('j'キー)で切り替え
  詳しくはreadme-sub.txtを参照

- 外部ライブラリlibdemulti2によるMULTI2復号化
   dvb://だけでなくTSファイル再生にも復号.
  (libdemulti2は配布していない。 APIについてはヘッダファイルdemulti2.hを参照)

## 本パッチの使い方・ビルド手順 (Fedoraの場合) ##
 レポジトリからパッチ済ソースのブランチ("isdb")を入手する。
 なお本ブランチは mplayer r36482 (gitだとcommit cb6ee05b)をベースとしたパッチとなる

  mplayerが内部で使用するffmpegについては、commit 6abb9eb5を使用したが、
  こちらはレビジョンを合わせなくても最新版でも問題ないはず.

1. 必要なパッケージのインストール
 - デスクランブルを行う場合
   libdemulti2 と libyakisoba{,-devel} か libsobacas{,-devel} か pcsc-lite{,-devel}

 - 字幕機能を使う場合
   fontconfig{,-devel} と harfbuzz{,-devel} と freetype{,-devel}

2. ソースの準備(mplayer)

 a) パッチ済ソースを展開

        git clone [--depth 1] https://github.com/0p1pp1/mplayer.git

 あるいは
 b) 既に以前に上記a) git clone ... を実行済で新規の差分だけ入手する場合

        cd mplayer
        git pull origin isdb

 あるいは
 c) 既にmplayer本家のレポジトリのcloneがある場合 (未確認)

        cd mplayer
        git remote add isdb https://github.com/0p1pp1/mplayer.git
        git pull isdb isdb

3. ソースの準備(ffmpeg) : 省略可。ただし省略すると後の手順で時間がかかる。
 FFmpegのwebページからソースを持ってきて ffmpeg/に展開して置く。

        cd mplayer;
        wget http://ffmpeg.mplayerhq.hu/releases/ffmpeg-2.1.tar.bz2
        tar xjvf ffmpeg-2.1.tar.bz2
        mv ffmpeg-2.1 ffmpeg

4. mplayerのconfigure

        (例) ./configure --prefix=/usr --confdir=/etc --libdir=/usr/lib64 \
                 --enable-ass-internal --enable-menu

  上の手順3.を省いていると、
  configureの途中でffmpegのソースを入手するか聞いてくるので、ENTER で了承する。
  git clone --depth 1 ... でffmpegから持ってくるのでやや時間がかかる。

  - デスクランブル関連の機能については、libdemulti2がインストールされているか検出され、
    利用可能であれば自動的に使用するよう設定されている。
    ヘッダファイルやライブラリが標準の場所にインストールされていなければ、
    適宜CFLAGSやLDFLAGSなどを設定して./configureを実行する必要があある。

    また以下のconfigureオプションで、明示的にデスクランブル機能を使わないよう指示できる。
    + --disable-bcas デスクランブル機能を組み込まない場合

   - 字幕機能が要らない場合は--enable-ass-internal は必要ない

5. ビルド

   make

6. 実行

  (例) ./mplayer ~/foo.ts
