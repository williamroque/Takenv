import smtplib
import datetime
import os

# [Plagiarized] from stackabuse.com

gmail_user = 'william.roque@mascamarena.es'
gmail_password = 'Etquomadriserit16'

sfrom = gmail_user
to = 'william.aroque@gmail.com'
subject = 'vimrc backup ' + str(datetime.datetime.now())

vimrc_path = os.path.expanduser('~/.vimrc')
with open(vimrc_path, 'r') as vrc:
    vimrc = vrc.read()

email_text = """\
From: %s
To: %s
Subject: %s

%s
""" % (sfrom, to, subject, vimrc)

try:
    server = smtplib.SMTP_SSL('smtp.gmail.com', 465)
    server.ehlo()
    server.login(gmail_user, gmail_password)
    server.sendmail(sfrom, to, email_text)
    server.close()
except:
    print('Backup failed')
