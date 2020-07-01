import datetime
import subprocess
import signal

start = datetime.datetime.now()
fileName = 'log-' + start.strftime('%Y%m%d_%H%M%S') + '.txt'
traceroute = ['sudo','traceroute','-T','-q','1','aws.amazon.com']
ping = ['ping','-c','1','-q','-s','32','aws.amazon.com']

interval = 15
timeRange = 3600
count = 0
limit = timeRange/interval

f = open(fileName,'w')

def schedule(arg1,arg2):
	time = 'datetime ' + datetime.datetime.now().strftime('%Y/%m/%d %H:%M:%S')
	tracerouteResult = subprocess.check_output(traceroute)
	pingResult = subprocess.check_output(ping)
	f.write(time +'\n' + tracerouteResult +'\n' + pingResult)

	global count
	count += 1

def main():
	signal.signal(signal.SIGALRM,schedule)
	signal.alarm(1)
	signal.setitimer(signal.ITIMER_REAL, 15, 15)

	while(count < limit):
		''

	f.close()
	exit()
		
if __name__ == '__main__':
	main()
