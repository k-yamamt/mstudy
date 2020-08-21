from datetime import datetime
import time
import subprocess
import signal
from concurrent import futures

start = datetime.now()
fileName = 'exam1.txt'
ping = 'ping -c 1 -q -n -s 32 13.231.224.193'

interval = 1
count = 0

f = open(fileName,'w')

pool = futures.ThreadPoolExecutor(max_workers=100)

def rtt():
	t = datetime.now()
	f.write(t.strftime("%F %T") + '.' + str(t.microsecond) + '\n' + subprocess.check_output(ping,shell=True))

def schedule(arg1,arg2):
	for i in range(100):
		time.sleep(0.01)
		pool.submit(rtt)
	global count
	count = count + 1

def main():
	signal.signal(signal.SIGALRM,schedule)
	signal.setitimer(signal.ITIMER_REAL, interval, interval)

	while(count < 1200):
		''

	f.close()
	exit()

if __name__ == '__main__':
	main()
