from datetime import datetime
import subprocess
import signal

start = datetime.now()
fileName = 'log-' + start.strftime('%m%d_%H%M') + '.txt'
ping = 'ping -c 1 -q -n -s 32 13.231.224.193'

interval = 15

f = open(fileName,'w')

def schedule(arg1,arg2):
	time = datetime.now().strftime('%F %T')

	try:
		pingResult = subprocess.check_output(ping,shell = True)
		f.write(time +'\n' + pingResult)
	except:
		f.close()
		exit()

def main():
	signal.signal(signal.SIGALRM,schedule)
	signal.setitimer(signal.ITIMER_REAL, interval, interval)

	while(True):
		''

	f.close()
	exit()

if __name__ == '__main__':
	main()
