
# 1. Create reposity on github https://github.com/big3k/uncw_rain_data.git
#    with README.md created there. 

# 2.
cd /data1
git init
git clone https://github.com/big3k/uncw_rain_data1.git

# check in code from any subdirectory 
cd tiany/MRMS/CONUS
git add download_*.sh

git commit

# first push to GitHub
git push --set-upstream https://github.com/big3k/uncw_rain_data1.git master

# subsequent push only needs to run
# git push


10874  2024-03-16 14:22:28 git push
10875  2024-03-16 14:22:42 git push https://github.com/big3k/uncw_rain.git
10876  2024-03-16 14:23:02 git push --set-upstream https://github.com/big3k/uncw_rain.git master
10877  2024-03-16 14:24:09 git config --global user.name big3k
10878  2024-03-16 14:24:16 git push --set-upstream https://github.com/big3k/uncw_rain.git master
10879  2024-03-16 14:24:30 git config --global user.name tiany
10880  2024-03-16 14:24:46 git config --global user.email tiany@uncw.edu
10881  2024-03-16 14:24:53 git status
10882  2024-03-16 14:26:41 history |grep qdwwqxwn
10883  2024-03-16 14:27:10 ssh -T big3k@github.com
10884  2024-03-16 14:28:31 ssh -T qdwwqxwn@github.com
10885  2024-03-16 14:28:37 ssh -vT qdwwqxwn@github.com
10886  2024-03-16 14:28:48 git status
10887  2024-03-16 14:28:52 history |grep git
10888  2024-03-16 14:30:26 /usr/bin/gh auth git-credential
10889  2024-03-16 14:31:25 gh auth login
10890  2024-03-16 14:43:00 history
10891  2024-03-16 14:43:06 git push --set-upstream https://github.com/big3k/uncw_rain.git master
10892  2024-03-16 14:45:10 cd ..
10893  2024-03-16 14:45:10 ls
10894  2024-03-16 14:45:12 cd ..
10895  2024-03-16 14:45:12 ls
10896  2024-03-16 14:45:21 cd Downscale/
10897  2024-03-16 14:45:22 ls
10898  2024-03-16 14:45:27 cd deconvolution/
10899  2024-03-16 14:45:28 ls
10900  2024-03-16 14:45:30 git add *.m
10901  2024-03-16 14:45:33 git commit
10902  2024-03-16 14:46:07 git push
10903  2024-03-16 14:47:00 history

