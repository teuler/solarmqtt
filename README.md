# solarmqtt

[<img src="https://github.com/teuler/solarmqtt/blob/main/pics/scr02.png" align="right" alt="Drawing" width="320"/>](https://github.com/teuler/solarmqtt/blob/main/pics/scr02.png)

Display für Solar-Balkonkraftwerk mit Pico und MQTT. Läuft auf einem [Raspberry Pi Pico W](https://www.raspberrypi.com/products/raspberry-pi-pico/) unter [MMBasic](https://mmbasic.com/) in der [Webmite](https://geoffg.net/webmite.html)-Version. Interpretiert MQTT-Nachrichten über die erzeugte Solarenergie, hier vom in [Home Assistant](https://www.home-assistant.io/) eingebauten MQTT-Broker.

## Material
- Einen [Raspberry Pi Pico W](https://www.raspberrypi.com/products/raspberry-pi-pico/), z.B. von [Reichelt](https://www.reichelt.de/de/de/raspberry-pi-pico-w-rp2040-cortex-m0-wlan-microusb-rasp-pi-pico-w-p329646.html?PROVID=2788&gclid=EAIaIQobChMIuuXRwPKs_wIVTxUGAB0hJw2sEAQYASABEgL6TPD_BwE&&r=1)
- Ein [WaveShare 2.8" 320x240 Touch Display Module für Raspberry Pi Pico](https://www.waveshare.com/wiki/Pico-ResTouch-LCD-2.8) (262K Farben, SPI), z.B. von [Eckstein](https://eckstein-shop.de/WaveShare28inchTouchDisplayModuleforRaspberryPiPico2C262KColors2C320C3972402CSPI)

<!-- Pro Wechselrichter einen [Shelly Plug S](https://www.shelly.cloud/en/products/shop/shelly-plug-s) oder einen vergleichbaren, Home Assistant-kompatiblen Zwischenstecker zur Leistungsmessung.-->

## House assistant

**Auslöser - Time trigger pattern**
```yaml
platform: time_pattern
seconds: /10
enabled: true
```

**Aktion(en) - MQTT Publish**
```yaml
service: mqtt.publish
data:
  qos: 0
  retain: false
  topic: solar/panels
  payload: >-
    name:  
    [o,u]; 
    pw:
    [{{states.sensor.shellyplug2_solar_1_power.state}},
     {{states.sensor.shellyplug2_solar_2_power.state}}];
    pw-unit:
    {{states.sensor.shellyplug2_solar_1_power.attributes.unit_of_measurement}};
    en: 
    [{{states.sensor.shellyplug2_solar_1_energy.state | round(3)}},
     {{states.sensor.shellyplug2_solar_2_energy.state | round(3)}}];
    en-unit:
    {{states.sensor.shellyplug2_solar_1_energy.attributes.unit_of_measurement}};
    overpw: 
    [{{ int(is_state("binary_sensor.shellyplug2_solar_1_overpowering", "on")) }},
     {{ int(is_state("binary_sensor.shellyplug2_solar_2_overpowering", "on")) }}]; 
    overht: 
    [{{int(is_state("binary_sensor.shellyplug2_solar_1_overheating", "on")) }},
     {{int(is_state("binary_sensor.shellyplug2_solar_2_overheating", "on")) }}];
```
