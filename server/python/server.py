from twisted.internet.protocol import Factory
from twisted.protocols.basic import LineReceiver
from twisted.internet import reactor
from operator import add

class FileServer(LineReceiver):
	delimiter = '\n'

	def __init__(self, filesystem, locks, idnum):
		self.filesystem = filesystem
		self.locks = locks
		self.idnum = idnum
		self.fds = {}
		self.pos = {}
		self.fd = 0

	def isFileLocked(self, name):
		return name in reduce(add, self.locks.itervalues())

	def connectionMade(self):
		self.locks[self.idnum] = []

	def connectionLost(self, reason):
		del self.locks[self.idnum]

	def lineReceived(self, line):
		getattr(self, "handle_{0}".format(line[:3]))(line)

	def handle_CON(self, message):
		self.sendLine("OK ID {0}".format(self.idnum))

	def handle_LSD(self, name):
		files = " ".join(self.filesystem.keys())
		self.sendLine("OK {0}".format(files))

	def handle_DEL(self, message):
		cmd, name = message.split(' ', 1)
		try:
			del self.filesystem[name]
			self.sendLine("OK")
		except KeyError:
			self.sendLine("ERROR 1337 ENOF")

	def handle_CRE(self, message):
		cmd, name = message.split(' ', 1)
		self.filesystem[name] = ""
		self.sendLine("OK")

	def handle_OPN(self, message):
		cmd, name = message.split(' ', 1)
		if self.isFileLocked(name):
			self.sendLine("ERROR 1338 EOPENALREADY")
			return
		self.fd += 1
		self.fds[self.fd] = name
		self.pos[self.fd] = 0
		self.locks[self.idnum].append(name)
		self.sendLine("OK FD {0}".format(self.fd))

	def handle_WRT(self, message):
		cmd, a, fd, b, size, content = message.split(' ', 5)
		name = self.fds[int(fd)]
		self.filesystem[name] = "{0}{1}".format(self.filesystem[name], content)
		self.sendLine("OK")

	def handle_REA(self, message):
		cmd, a, fd, b, size = message.split(' ', 4)
		name = self.fds[int(fd)]
		pos = self.pos[int(fd)]
		try:
			content = self.filesystem[name][pos:(pos+int(size))]
			self.pos[int(fd)] = pos + int(size)
		except KeyError:
			try:
				content = self.filesystem[name][pos:]
				self.pos[int(fd)] = pos + len(content)
			except KeyError:
				content = ""
				size = 0
		self.sendLine("OK SIZE {0} {1}".format(len(content), content))

	def handle_CLO(self, message):
		cmd, a, fd = message.split(' ', 2)
		self.locks[self.idnum].remove(self.fds[int(fd)])
		del self.fds[int(fd)]
		del self.pos[int(fd)]
		self.sendLine("OK")

	def handle_BYE(self, message):
		self.sendLine("OK")

class FileServerFactory(Factory):

	def __init__(self):
		self.filesystem = {} # maps names to file contents
		self.locks = {}
		self.idnum = 0

	def buildProtocol(self, addr):
		self.idnum += 1
		return FileServer(self.filesystem, self.locks, self.idnum)


reactor.listenTCP(8000, FileServerFactory())
reactor.run()
