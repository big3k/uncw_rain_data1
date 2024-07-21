#! /bin/bash 

grads='/data1/software/grads-2.2.1/bin/grads -d X11 -h GD'

$grads -bl <<EOF
open isfcrain
set grads off
set gxout shaded
d isfcrain*0.01
cbarn
draw title isfcrain*0.01
printim img_isfcrain.gif gif white x1200 y800
quit
EOF

