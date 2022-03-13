64tass -a src\super8086.asm -l target\super8086.lbl -L target\super8086.lst -o target\super8086
cd target
c1541 -attach super8086.d64 -delete super8086
c1541 -attach super8086.d64 -write super8086 super8086
cd ..