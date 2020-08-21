from ftplib import FTP
import time

try:
	ftp = FTP('13.231.224.193')
	ftp.set_pasv(False)
	time.sleep(1)
	ftp.login('hitachi','hitachi2020')
	time.sleep(1)
	with open('1k.dat', 'rb') as fp:
		ftp.storbinary('STOR /1k.dat', fp)
	ftp.quit()
except:
	''
