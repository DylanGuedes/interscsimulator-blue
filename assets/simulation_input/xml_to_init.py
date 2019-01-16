import xml.etree.ElementTree as ET

if __name__ == '__main__':
    XML_PATH = "./trips_completo.xml"
    f = open("ex.init", "w+")

    gen_id = 1
    tree = ET.parse(XML_PATH).getroot()
    for child in tree:
        attrs = child.attrib
        for v in attrs["count"]:
            payload = """
            {{ class_Car, [#{{id => "{}", origin => "{}", destination => "{}", start_time => {}, start_link => "{}" }}] }}.
            """.format(gen_id, attrs["origin"], attrs["destination"], int(attrs["start"]), attrs["link_origin"])
            f.write(payload)
            gen_id += 1
    f.close()
