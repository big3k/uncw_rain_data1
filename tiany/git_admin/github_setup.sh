
# 1. On GitHub, ereate reposity https://github.com/big3k/uncw_rain_data1.git
#    with README.md created there. 

# 2. On local machine: 

cd /data1
git init

git config core.sharedRepository group

# let other group memebers have write permission to the repository 
chmod -R g+w .git 

git config --global user.email "tiany@uncw.edu"
git config --global user.name "tiany"

git clone https://github.com/big3k/uncw_rain_data1.git

# On GitHub, create "Personal access tokens (classic)", and save to ~/.netrc

machine github.com login tiany password ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

chmod go-r ~/.netrc 

# check in code from any subdirectory 
cd tiany/MRMS/CONUS
git add download_*.sh

git commit

# first push to GitHub
git push --set-upstream https://github.com/big3k/uncw_rain_data1.git master

# subsequent push only needs to run
# git push

#--------------------------------------------------------------------
#To let other users of the same GID check-in their code to the same repository 
# Create a separate "Personal access tokens (classic)" for each user

git config --global user.email "test_user@uncw.edu"
git config --global user.name "test_user"

echo "machine github.com login tiany password ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" > ~/.netrc
chmod go-r ~/.netrc 

cd /data1/test_user
echo "My branch" > README.md 

git add README.md

git commit README.md -m "My branch" 
git push --set-upstream https://github.com/big3k/uncw_rain_data1.git master


