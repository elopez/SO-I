import socket
import unittest

TCP_ADDR = '127.0.0.1'
TCP_PORT = 8000

class ServerTest(unittest.TestCase):
	s = None
	def connect_to_server(self):
		self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		self.s.connect((TCP_ADDR, TCP_PORT))

	def send(self, data):
		self.s.send(data.encode('UTF-8'))

	def recv(self):
		data = ""
		while "\n" not in data:
			data += self.s.recv(4096).decode('UTF-8')
		return data

	def do_handshake(self):
		self.send("CON\n")
		self.recv()

	def create_file(self, name):
		self.send("CRE " + name + "\n")
		self.recv()

	def disconnect(self):
		self.send("BYE\n")
		self.s.close()
