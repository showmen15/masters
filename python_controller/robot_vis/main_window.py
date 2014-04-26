__author__ = 'michal'

from PyQt4 import QtGui, QtCore

class MainWindow(QtGui.QWidget):

    def __init__(self, server):
        super(MainWindow, self).__init__()

        self._server = server

        self.initUI()

    def initUI(self):
        groupbox = QtGui.QGroupBox("Simulation")
        groupbox_layout = QtGui.QVBoxLayout()

        start_button = QtGui.QPushButton("Start")
        start_button.clicked.connect(self.start_clicked)

        stop_button = QtGui.QPushButton("Stop")
        stop_button.clicked.connect(self.stop_clicked)

        reset_button = QtGui.QPushButton("Reset")
        reset_button.clicked.connect(self.reset_clicked)

        groupbox_layout.addWidget(start_button)
        groupbox_layout.addWidget(stop_button)
        groupbox_layout.addWidget(reset_button)
        groupbox_layout.addStretch(1)

        groupbox.setLayout(groupbox_layout)

        main_layout = QtGui.QVBoxLayout()
        main_layout.addWidget(groupbox)
        self.setLayout(main_layout)

        self.setWindowTitle("Simulation control")
        self.show()

    def start_clicked(self):
        self._server.send_message("start")

    def stop_clicked(self):
        self._server.send_message("stop")

    def reset_clicked(self):
        self._server.send_message("reset")

    def closeEvent(self, event):
        QtGui.QApplication.quit()

