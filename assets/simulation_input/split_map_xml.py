import xml.etree.ElementTree as ET
import sys

if __name__ == '__main__':
    if (len(sys.argv) < 3):
        raise(Exception("Usage: python3 split_map_xml.py map_path.xml output_name"))

    map_path = sys.argv[1]
    output_name = sys.argv[2];

    f_nodes = open(output_name+"nodes.xml", "w+")
    f_links = open(output_name+"links.xml", "w+")

    tree = ET.parse(map_path).getroot()
    for child in tree:
        if (child.tag == "nodes"):
            f_nodes.write("""<nodes>\n""")
            for node in child:
                attrs = node.attrib
                payload = """
                <node id="{0}" x="{1}" y="{2}"/>
                """.format(attrs["id"], attrs["x"], attrs["y"])
                f_nodes.write(payload)
            f_nodes.write("""</nodes>\n""")
        if (child.tag == "links"):
            f_links.write("""<links>\n""")
            for node in child:
                attrs = node.attrib
                payload = """
                <link id="{0}" from="{1}" to="{2}" length="{3}" freespeed="{4}" capacity="{5}" permlanes="{6}" oneway="{7}" modes="{8}" origid="{9}" type="{10}"/>
                """.format(attrs["id"], attrs["from"], attrs["to"], attrs["length"], attrs["freespeed"], attrs["capacity"], attrs["permlanes"], attrs["oneway"], attrs["modes"], attrs["origid"], attrs["type"])
                f_links.write(payload)
            f_links.write("""</links>\n""")
    f_nodes.close()
    f_links.close()
