import rospy
import xacro
from giskard_msgs.msg import WorldBody
import os.path
from giskardpy import tfwrapper as tf
from giskardpy.urdf_object import URDFObject
from giskardpy.utils import resolve_ros_iris
from knowrob_refills.knowrob_wrapper import KnowRob

rospy.init_node('facing_filler')
tf.init(5)

knowrob = KnowRob(
    initial_mongo_db=None,
    # initial_mongo_db='/home/stelter/mongo_logs/2020-12-18_18-12-01/roslog',
    # initial_mongo_db='/home/stelter/mongo_logs/2020-12-18_18-12-012/',
    # initial_mongo_db='/home/stelter/mongo_logs/2021-01-21_18-13-08/roslog',
    clear_roslog=False,
    # clear_roslog=True,
    # republish_tf=True,
    neem_mode=False)


empty = False

for shelf_id in knowrob.get_shelf_system_ids(False):
    for layer_id in knowrob.get_shelf_layer_from_system(shelf_id, filter=False):
        for facing_id in knowrob.get_facing_ids_from_layer(layer_id):
            if knowrob.is_facing_empty(facing_id):
                knowrob.add_objects(facing_id, 1, None)
                print('filled {}'.format(facing_id))
    # break

knowrob.save_neem('/home/stelter/mongo_logs/whole_store_filled_neem')
knowrob.mongo_dump_database('/home/stelter/mongo_logs/whole_store_filled')
print('saved file. done.')
