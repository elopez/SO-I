import random
import unittest
from utils.client import ServerTest

class TestBasicFunctionality(ServerTest):
	def setUp(self):
		self.connect_to_server()

	# CON should give us an ID
	def test_con(self):
		self.send("CON\n")
		self.assertTrue(self.recv().startswith("OK ID "))

	# CON is only valid as first operation
	def test_double_con(self):
		self.do_handshake()
		self.send("CON\n")
		self.assertEqual(self.recv(), "ERROR 71 EPROTO\n")

	# CON is the only operation valid when starting
	def test_invalid_con(self):
		self.send("HEY\n")
		reply = self.recv()
		self.assertEqual(reply, "ERROR 71 EPROTO\n")

	# LSD on an empty filesystem should work
	def test_lsd_empty(self):
		self.do_handshake()
		self.send("LSD\n")
		reply = self.recv()
		self.assertEqual(reply, "OK \n")

	def test_cre_lsd_del(self):
		self.do_handshake()
		# Files can only be created once
		self.send("CRE testing1\n")
		self.assertEqual(self.recv(), "OK\n")
		self.send("CRE testing1\n")
		self.assertEqual(self.recv(), "ERROR 17 EEXIST\n")
		# LSD should list the files
		self.send("LSD\n")
		self.assertEqual(self.recv(), "OK testing1\n")
		self.send("CRE testing2\n")
		self.assertEqual(self.recv(), "OK\n")
		self.send("LSD\n")
		self.assertTrue(self.recv() in ["OK testing1 testing2\n", "OK testing2 testing1\n"])
		# Files can only be deleted once
		self.send("DEL testing1\n")
		self.assertEqual(self.recv(), "OK\n")
		self.send("DEL testing1\n")
		self.assertEqual(self.recv(), "ERROR 2 ENOENT\n")
		self.send("DEL testing2\n")
		self.assertEqual(self.recv(), "OK\n")

	def tearDown(self):
		self.send("BYE\n")
