# solarmqtt
Display für Solar-Balkonkraftwerk mit Pico und MQTT 


## House assistant

**Auslöser - Time trigger pattern**
```
platform: time_pattern
seconds: /10
enabled: true
```

**Aktion(en) - MQTT Publish**
```
service: mqtt.publish
data:
  qos: 0
  retain: false
  topic: solar/panels
  payload: >-
    name:  [o,u]; pw:
    [{{states.sensor.shellyplug2_solar_1_power.state}},{{states.sensor.shellyplug2_solar_2_power.state}}];
    pw-unit:
    {{states.sensor.shellyplug2_solar_1_power.attributes.unit_of_measurement}};
    en: [{{states.sensor.shellyplug2_solar_1_energy.state |
    round(3)}},{{states.sensor.shellyplug2_solar_2_energy.state | round(3)}}];
    en-unit:
    {{states.sensor.shellyplug2_solar_1_energy.attributes.unit_of_measurement}};
    overpw: [{{ int(is_state("binary_sensor.shellyplug2_solar_1_overpowering",
    "on")) }},{{ int(is_state("binary_sensor.shellyplug2_solar_2_overpowering",
    "on")) }}]; overht: [{{
    int(is_state("binary_sensor.shellyplug2_solar_1_overheating", "on")) }},{{
    int(is_state("binary_sensor.shellyplug2_solar_2_overheating", "on")) }}];
```
