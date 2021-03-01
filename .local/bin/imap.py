
#!/usr/bin/python

import imaplib
obj = imaplib.IMAP4_SSL('imap.gmail.com',993)
<<<<<<< HEAD
obj.login('USER@gmail.com','PASSWD') # write your email and password
=======
obj.login('youremail@gmail.com','passwd') # write your email and password
>>>>>>> 2e939be3c0d913290c07506aaa5518f97e836ced
obj.select()
print(len(obj.search(None, 'UnSeen')[1][0].split()))
