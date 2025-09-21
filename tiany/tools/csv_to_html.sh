# Create html out from input csv file. 
# Assuming the first line is header
# e.g., input: 
#
#Model, loss_f, act_f, pre/post-training, Bias, RMSE, Corr
#model_1ax, ssim_loss, relu, pre, -0.162,2.371, 0.765
#model_1ax, ssim_loss, relu, post,-0.109, 2.223, 0.793
#model_2ax, ssim_loss, relu, pre, -0.162,2.371, 0.765
#model_2ax, ssim_loss, relu, post,-0.155, 2.280, 0.781

awk -F',' '
BEGIN {
    print "<table border=\"1\" class=\"table table-striped\">"
}
NR == 1 {
    print "  <thead><tr>"
    for (i = 1; i <= NF; i++) {
        print "    <th>" $i "</th>"
    }
    print "  </tr></thead>"
    print "  <tbody>"
    next
}
{
    print "  <tr>"
    for (i = 1; i <= NF; i++) {
        print "    <td>" $i "</td>"
    }
    print "  </tr>"
}
END {
    print "  </tbody>"
    print "</table>"
}
' $1

