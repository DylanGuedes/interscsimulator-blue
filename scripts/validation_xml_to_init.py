import xml.etree.ElementTree as ET
import sys
import uuid
from interscity_client import platform
from multiprocessing import Process, Pool
import os
import requests


def append_trip(attrs, uuid_to_use, gen_id):
    mydata = {
        'data': {
            'uuid': uuid_to_use,
            "description": "car with uuid {0}".format(uuid_to_use), "capabilities": ["city_traffic"],
            "lat": -23,
            "lon": -46,
            "status": "active"
        }
    }
    requests.post("http://localhost:8000/adaptor/components", json=mydata)
    NODES = ['a']
    nodes_len = len(NODES)
    return """{{ class_Car, [#{{id => "{}", origin => "{}", destination => "{}", start_time => {}, start_link => "{}", uuid => "{}" }}], {} }}.\n""".format(gen_id, attrs["origin"], attrs["destination"], attrs["start"], attrs["link_origin"], uuid_to_use, NODES[(gen_id%nodes_len)])

if __name__ == '__main__':

    running_env = os.getenv("RUNNING_ENV")

    xml_path = "local"
    init_path = "./trips.init"
    if (running_env=="docker_test"):
        xml_path = "./validation/trips.xml"
        init_path = "./validation/docker_trips.init"
    elif (running_env=="production"):
        xml_path = "./production/trips.xml"
        init_path = "./production/trips.init"
    else:
        raise("Please, define the env variable `RUNNING_ENV` with either `docker_test` or `production`")

    f = open(init_path, "a+")
    conn = platform.connection()
    if not conn.capability_available("city_traffic"):
        conn.create_capability("city_traffic", "City Traffic", "sensor")

    gen_id = 1

    tree = ET.parse(xml_path).getroot()
    p = Pool(12)
    for child in tree:
        attrs = child.attrib
        count_to_use = int(attrs["count"])

        cluster_trips = list(map(lambda x: (x, str(uuid.uuid4())), [attrs]*count_to_use))
        cluster_trips_with_idx = [(x[0], x[1], gen_id+idx) for idx, x in enumerate(cluster_trips)]
        results = p.starmap(append_trip, cluster_trips_with_idx)
        gen_id += count_to_use
        for u in results:
            f.write(u)
        sys.stdout.write('.')
    f.close()
