import xml.etree.ElementTree as ET
import sys
import uuid
from interscity_client import platform

if __name__ == '__main__':
    XML_PATH = "./sao_paulo/trips_completo.xml"
    f = open("saopaulo_trips.init", "a+")
    nodes = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm']
    computing_nodes = 13
    conn = platform.connection()
    if not conn.capability_available("city_traffic"):
        conn.create_capability("city_traffic", "City Traffic", "sensor")

    car_builder = platform.resource_builder(connection=conn, capability="city_traffic", uniq_key="uuid")

    gen_id = 1
    tree = ET.parse(XML_PATH).getroot()
    for child in tree:
        attrs = child.attrib
        count_to_use = int(int(attrs["count"])/8)
        for v in range(0, count_to_use):
            generated_uuid = str(uuid.uuid4())
            car_builder.register(generated_uuid, "Car "+generated_uuid, ["city_traffic"])
            payload = """
            {{ class_Car, [#{{id => "{}", origin => "{}", destination => "{}", start_time => {}, start_link => "{}", uuid => "{}" }}], {} }}.
            """.format(gen_id, attrs["origin"], attrs["destination"], attrs["start"], attrs["link_origin"], generated_uuid, nodes[(v%computing_nodes)])
            f.write(payload)
            gen_id += 1
        sys.stdout.write('.')
    f.close()
