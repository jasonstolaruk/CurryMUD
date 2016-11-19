mudlet = mudlet or {}; mudlet.mapper_script = true

function curry_mapper()
  info = gmcp.Room.Info
  display(info)

  local area_id = find_area_id(info.area_name)
  if not area_id then area_id = addAreaName(info.area_name) end
  addRoom(info.room_id)
  setRoomArea(info.room_id, area_id)
  setRoomCoordinates(info.room_id, info.x_coord, info.y_coord, info.z_coord)
  -- setExit(info.last_room_id, info.rm_id,
  centerview(info.room_id)
end

function find_area_id(name)
  for area, id in pairs(getAreaTable()) do
    if area:find(name, 1, true) then return id end
  end
end
