
## Schematy ##
[Schemat symulacji](http://i.imgur.com/rDLZe2k.png)

[Schemat algorytmu na robocie](http://i.imgur.com/cBP1wVj.png)

## Requirements ##

```
python
rebar (https://github.com/basho/rebar) [git clone https://github.com/basho/rebar.git]
logiviw (https://pythonhosted.org/logview/)

apt-get install erlang
apt-get install python-enum34
apt-get install python-minimal
apt-get install gnuradio
pip install pymongo
pip install pyzmq


Dodaj do $PATH
rebar
logview
```

##  Requirements Robot ##
```
apt-get install python-numpy
apt-get install python-enum
pip install scipy
pip install --upgrade setuptools
pip install scipy
easy_install pykalman
apt-get install liblapack-dev
easy_install numpy scipy Sphinx numpydoc nose pykalman
apt-get install python-qt4

```


## Kompilowanie aplikacji erlangowych ##

```
cd robot_controller
rebar get-deps compile
```

## Uruchamianie symulacji ##

### Kroki na maszynie wirtualnej ###
1. Uruchom program RoBOSS Kontroler
2. Wybierz model środowiska (np. `3vs1.xml`)
3. Uruchom symulator (`start local simulator`)
4. Uruchom interfejs erlangowy 
```
werl.exe -pa apps/roboss/ebin -pa deps/protobuffs/ebin/ -sname roboss -setcookie agh -eval application:start(roboss)
```

### Na maszynie lokalnej ###

1 . Uruchom aplikację wizualizującą

```
cd python_controller/robot_vis
python server.py ../../models/3vs1.roson
```

2 . Uruchom program [logview](https://pythonhosted.org/logview/) 

3 . Uruchom klienta 
```
cd robot_controller
./start_client.sh 3vs1
```

## Uruchamianie algorytmu na robotach ##

### Kroki do wykonania na centralnym zarządcy ###

1 . Uruchom `state_manager`

```
cd robot_controller
./start_state_manager.sh
```
2 . Konfiguracja

```
/etc/hosts

127.0.0.2       sm 

```
### Kroki do wykonania na maszynie monitorującej ###
1 . Uruchom aplikację wizualizującą

```
cd python_controller/robot_vis
python server.py ../../models/3vs1.roson
```

2 . Uruchom program [logview](https://pythonhosted.org/logview/) 

### Kroki do wykonania na robocie ###

1 . Uruchom `amber`. Upewnij się, że aktywne są sterowniki `location`, `hokuyo` i `roboclaw`.

2 . Konfiguracja

```
/etc/hosts

<IP komputera z uruchomionym procesem state manager>       sm 

3 . Uruchom algorytm
```
cd robot_controller
./start_amber_client.sh 3vs1
