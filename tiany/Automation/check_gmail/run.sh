
cd /data1/tiany/Automation/check_gmail
./gmail_check_from_headless.py --query "subject:shipped" --unread  > /data1/web/lswg/group/check_gmail/received.txt

#./gmail_check_from_headless.py --from "noreply@noaa.gov" --unread --max 10
