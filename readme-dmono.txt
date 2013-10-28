二重音声切り替えのユーザI/Fについて

  二重音声や二ヶ国語音声の場合に 自動的に主音声を再生するとともに
  "switch_dmono"コマンドの発行や "dual_mono"プロパティの設定によって
  音声を切り替えられる機能を追加した.
   また自動的に再生する際のデフォルトの選択は "dualmono"オプションで
  指定される. (0: Main/L, 1: Sub/R, 2: Both/LR,  default: 0)

切り替え方の例:
1) input.confに以下の行を追加すると'A'キーで切り替わる.(主->副->主副)
A  switch_dmono

 現在のキーバインドについては man mplayer(1) とか
 [/usr/local]/etc/mplayer/input.confを参照

2) mplayer -slaveをつけて slaveモードで起動し, stdinから
  "switch_dmono"と入力するか "set_property dual_mono 1" と入力する.

3) mplayer -dualmono 1 とするか configファイルに"dualmono=1"の行を
  追加すると 二ヶ国語の場合に自動的に副音声の方を再生する

'#'キーでのswitch_audioコマンドで音声を切り替えた場合は、 例えば
    音声1 -> 音声2.main -> 音声2.sub -> 音声1
のように、別のストリームのように扱われる。
