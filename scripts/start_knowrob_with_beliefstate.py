#!/usr/bin/env python

import rospy

from knowrob_refills.knowrob_wrapper import KnowRob
# from refills_perception_interface.tfwrapper import lookup_pose

db = '~/mongo_logs/whole_store_filled_neem/'

rospy.init_node('test')
knowrob = KnowRob(initial_mongo_db=db,
                  clear_roslog=True,
                  republish_tf=True,
                  neem_mode=True)
# knowrob = KnowRob(initial_mongo_db=None,
#                   clear_roslog=False)
shelf_ids = knowrob.get_shelf_system_ids(False)
# print(shelf_ids)
for shelf_id in shelf_ids:
    print('shelf center frame id {}'.format(knowrob.get_object_frame_id(shelf_id)))
    print('shelf corner frame id {}'.format(knowrob.get_perceived_frame_id(shelf_id)))
    # print(knowrob.get_shelf_pose(shelf_id))
    # print(lookup_pose('map', perceived_frame_id))