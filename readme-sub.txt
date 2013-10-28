[EXPERIMENTAL] ISDB字幕について

日本のデジタルTV放送のTSストリームから字幕を取り出してASS形式に変換し、
mplayer既存のサブタイトル表示機能を使って表示する。
文字の表示位置や大きさ、色などできるだけ再現するようにしているが、
むりやりASSのマークアップを使って実現しているため、一部非対応の機能がある。
本来の機能については、ARIB STD-B24などを参照。

非対応機能：
・番組内容に無関係の字幕/図形「字幕スーパー」(降雨時アナウンス等)
・ロールアップモードの字幕テキスト
・外字、文字反転（フラッシング）、表示効果(ディゾルヴ、ワイプ等)
・一部の文字表示位置指定


使い方
a) TS再生中に字幕ON/OFF
 mplayer -ass dvb://NHK  --> 'j'キー(sub_demuxコマンド) で切り替え

b) 起動時に字幕の言語を指定
 mplayer -ass -slang jpn foo.ts

c) 起動時に字幕のPIDを指定
 mplayer -ass -sid 304 bar.ts

d) ASSを使わない素のテキストでの出力
 mplayer -utf8 dvb://NHK --> 'j'キー
  (表示位置や大きさ、色などすべて無視し画面下部に字幕文のテキストを表示)

e) 字幕のみを出力
 mplayer -dumpsub -dumpfile sub.ass -slang jpn zoo.ts


注意：
・切り替わり後に字幕が表示されるまでは若干の時間が必要（数秒〜10秒程度?）
・切り替えが可能になるまでに若干の時間が必要（数秒程度?）
   プログラム情報を読み込んで、字幕が格納されている場所を知る必要があるため


フォントについて
・日本語アウトラインフォント, fontconfigがインストールされていること
   特にデジタル放送で使われる特殊な文字を表示したい場合には、
   和田研中丸ゴシック2004ARIB(Unicode版)などARIB文字収録フォントが必要
   また字幕の表示がずれないために、等幅フォントであることが必要。

   そのようなフォントが下のコマンドで候補として挙がってくるようにするか、
       fc-match ':lang=ja:outline=true:spacing=dual:spacing=cell'
   希望のフォントを mplayerの-subfont オプションで指定する。


ビルド方法
ASSのバグや縦書き対応の修正のため、libassにかなりの変更が加えられている。
そのためmplayerのconfigure時に、外部のlibassでなく内部のlibassのコピー
を使うよう --enable-ass-internal オプションを付けて実行する必要がある。
  ./configure --enable-ass-internal .......

またfontconfig, freetype, harfbuzzもインストールされている必要がある。
