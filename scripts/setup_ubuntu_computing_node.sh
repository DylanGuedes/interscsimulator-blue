curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
mkdir -p $HOME/.bin
chmod a+x kerl
mv kerl $HOME/.bin/.
echo "export PATH=/home/ubuntu/.bin:\$PATH" >> $HOME/.bashrc
echo "source /home/ubuntu/kerl/20.0/activate" >> $HOME/.bashrc
echo "source /home/ubuntu/sim-diasca-blue/mock-simulators/interscsimulator-blue/src/update_environment_variables.sh" >> $HOME/.bashrc
sudo apt-get install gnuplot-x11 libssl-dev make automake libncurses5-dev gcc
source $HOME/.bashrc
kerl build 20.0 20.0
kerl install 20.0 $HOME/kerl/20.0
