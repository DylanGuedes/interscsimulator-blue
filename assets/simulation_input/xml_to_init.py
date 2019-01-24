import xml.etree.ElementTree as ET
import sys

if __name__ == '__main__':
    XML_PATH = "./sao_paulo/trips_completo.xml"
    f = open("saopaulo_trips.init", "a+")
    nodes = ['node1', 'node1', 'node2']
    computing_nodes = 3

    gen_id = 1
    tree = ET.parse(XML_PATH).getroot()
    for child in tree:
        attrs = child.attrib
        for v in range(0, int(attrs["count"])):
            payload = """
            {{ class_Car, [#{{id => "{}", origin => "{}", destination => "{}", start_time => {}, start_link => "{}" }}], {} }}.
            """.format(gen_id, attrs["origin"], attrs["destination"], 15, attrs["link_origin"], nodes[(v%computing_nodes)])
            f.write(payload)
            gen_id += 1
        sys.stdout.write('.')
    f.close()
