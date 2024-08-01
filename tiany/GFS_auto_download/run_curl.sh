
#8/1/2024
# How it works: 

# Step 1: submit the request

req_id=$$

curl -X POST \
   -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/png,image/svg+xml,*/*;q=0.8" \
   -d "satdisptype=N%2FA&stations=00&station_lst=&typeofdata=MODEL&dtypelist=&begdatestring=&enddatestring=&begyear=2008&begmonth=01&begday=01&beghour=&begmin=&endyear=2008&endmonth=01&endday=05&endhour=&endmin=&outmed=FTP&outpath=&pri=500&datasetname=GFS3&directsub=Y&emailadd=jianxiac%40gmail.com&outdest=FILE&applname=&subqueryby=STATION&tmeth=Awaiting-Data-Transfer" \
  https://www.ncei.noaa.gov/has/HAS.FileSelect  > response_${req_id}.txt


# The response will contain the "Order Number" field: 
#<tr><td class="var">Order Number:</td><td class="val">HAS012540680<a href="HAS.CheckOrderStatus?hasreqid=HAS012540680&emailadd=jianxiac@gmail.com" class="button completeButton statusButton noprint floatRight">Check order status</a></td></tr>
#

orderN=`grep "Order Number" response_${req_id}.txt | grep -Po 'hasreqid=\K.*?(?=&emailadd=)'`

# And when the data are ready, they will be located at as daily tar files: 
# https://www.ncei.noaa.gov/pub/has/model/HAS012540678/

# wait a while for data to be ready 

#wget -R "index.html" -nH -np -r https://www.ncei.noaa.gov/pub/has/model/HAS012540678/
# or parse out each individual tar file and download






