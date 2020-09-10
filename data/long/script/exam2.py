from datetime import datetime
import time
import subprocess
import signal
from concurrent import futures
from ftplib import FTP

start = datetime.now()
fileName = 'exam1.txt'
ftplog = 'exam1-ftplog.txt'
ping = 'ping -c 1 -q -n -s 32 13.231.224.193'

interval = 15
count = 0
limit = 80

ftp = FTP('13.231.224.193')
ftp.set_pasv(False)
time.sleep(1)
ftp.login('hitachi','hitachi2020')
time.sleep(1)

f = open(fileName,'w')
fl = open(ftplog,'w')

pool = futures.ThreadPoolExecutor(max_workers=15)

def ftp_put():
	t1 = datetime.now()
	with open('2M.dat', 'rb') as fp:
		result = ftp.storbinary('STOR /1k.dat', fp)
	t2 = datetime.now()
	fl.write(str(result) + ' : ' + t1.strftime("%T") + '.' + str(t1.microsecond) + ' - ' + t2.strftime("%T") + '.' + str(t2.microsecond) + '\n')

def rtt():
	t = datetime.now()
	f.write(t.strftime("%F %T") + '.' + str(t.microsecond) + '\n' + subprocess.check_output(ping,shell=True))

def schedule(arg1,arg2):
	pool.submit(ftp_put)
	time.sleep(0.5)
	for i in range(10):
		time.sleep(0.01)
		pool.submit(rtt)
	global count
	count = count + 1

def main():
	signal.signal(signal.SIGALRM,schedule)
	signal.setitimer(signal.ITIMER_REAL, interval, interval)

	while(count < limit):
		''
	
	time.sleep(10)
	f.close()
	fl.close()
	ftp.quit()
	exit()

if __name__ == '__main__':
	main()
