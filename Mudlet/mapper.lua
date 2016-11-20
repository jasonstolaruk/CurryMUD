mudlet = mudlet or {}; mudlet.mapper_script = true

function curry_mapper()
  local info = gmcp.Room.Info
  -- display(info)
  local area_id = find_area_id(info.area_name)
  if not area_id then
    area_id = addAreaName(info.area_name)
    setMapZoom(12)
  end
  if not find_room_id(info.room_id) then
    addRoom(info.room_id)
    setRoomName(info.room_id, info.room_name)
    setRoomArea(info.room_id, area_id)
    setRoomCoordinates(info.room_id, info.x_coord, info.y_coord, info.z_coord)
  end
  if find_room_id(info.last_room_id) then
    if info.last_room_id ~= info.room_id then
      if info.dir ~= -1 then
        setExit(info.last_room_id, info.room_id, info.dir)
      elseif info.special_dir ~= "-1" then
        addSpecialExit(info.last_room_id, info.room_id, info.special_dir)
      end
    end
  end
  centerview(info.room_id)
end

function find_area_id(name)
  for area, id in pairs(getAreaTable()) do
    if area:find(name, 1, true) then return id end
  end
end

function find_room_id(room_id)
  for id in pairs(getRooms()) do
    if id == room_id then return id end
  end
end
