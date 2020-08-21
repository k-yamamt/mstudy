#coding: utf-8
import subprocess
import os

observe = "ps ax | grep /home/pi/sample.py | grep -v grep |wc -l"
resume = "python /home/pi/sample.py"

try:
	result = subprocess.check_output(observe, shell=True).decode("utf-8").strip()

	if result != "2":
		subprocess.check_output(resume, shell=True)

except Excetption as e:
	''
