#include "dispatch.h"
/** Generic helpers **/

static void missingFieldError(lua_State *L, const char *field) {
  const char *desc = "No such field: ";
  char error[strlen(desc) + strlen(field) + 1];
  sprintf(error, "%s%s", desc, field);

  lua_pushstring(L, error);
  lua_error(L);
}

/** Top-level functions **/
int tl_RtTrackSideTgAngleL(lua_State *L) {
  tl_TrkLocPos *wrapper = (tl_TrkLocPos*) luaL_checkudata(L, 1, "torcs.TrkLocPos");
  tTrkLocPos *pos = wrapper->wrapped;
  tdble res = RtTrackSideTgAngleL(pos);
  lua_pushnumber(L, res);
  return 1;
}


/** tCarElt **/

int dispatch_CarElt(lua_State *L) {
  tl_CarElt *wrapper = (tl_CarElt *) luaL_checkudata(L, 1, "torcs.CarElt");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_CarElt");

  tCarElt *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_CarElt f = NULL;
  int i = 0;
  while (fields_CarElt[i].name != NULL && strcmp(fields_CarElt[i].name, field) != 0) {
    i++;
  }

  f = fields_CarElt[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_CarElt_index(lua_State *L, tCarElt *s) {
  lua_pushinteger(L, s->index);
  return 1;
}

int f_CarElt_info(lua_State *L, tCarElt *s) {
  tl_InitCar *wrapper = (tl_InitCar *) lua_newuserdata(L, sizeof(tl_InitCar));
  wrapper->wrapped = &(s->info);

  luaL_getmetatable(L, "torcs.InitCar");
  lua_setmetatable(L, -2);
  return 1;
}

int f_CarElt_pub(lua_State *L, tCarElt *s) {
  tl_PublicCar *wrapper = (tl_PublicCar *) lua_newuserdata(L, sizeof(tl_PublicCar));
  wrapper->wrapped = &(s->pub);

  luaL_getmetatable(L, "torcs.PublicCar");
  lua_setmetatable(L, -2);
  return 1;
}

int f_CarElt_priv(lua_State *L, tCarElt *s) {
  tl_PrivCar *wrapper = (tl_PrivCar *) lua_newuserdata(L, sizeof(tl_PrivCar));
  wrapper->wrapped = &(s->priv);

  luaL_getmetatable(L, "torcs.PrivCar");
  lua_setmetatable(L, -2);
  return 1;
}

/** tInitCar **/

int dispatch_InitCar(lua_State *L) {
  tl_InitCar *wrapper = (tl_InitCar *) luaL_checkudata(L, 1, "torcs.InitCar");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_InitCar");

  tInitCar *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_InitCar f = NULL;
  int i = 0;
  while (fields_InitCar[i].name != NULL && strcmp(fields_InitCar[i].name, field) != 0) {
    i++;
  }

  f = fields_InitCar[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_InitCar_name(lua_State *L, tInitCar *s) {
  lua_pushstring(L, s->name);
  return 1;
}

int f_InitCar_teamname(lua_State *L, tInitCar *s) {
  lua_pushstring(L, s->teamname);
  return 1;
}

int f_InitCar_carName(lua_State *L, tInitCar *s) {
  lua_pushstring(L, s->carName);
  return 1;
}

int f_InitCar_category(lua_State *L, tInitCar *s) {
  lua_pushstring(L, s->category);
  return 1;
}

int f_InitCar_raceNumber(lua_State *L, tInitCar *s) {
  lua_pushinteger(L, s->raceNumber);
  return 1;
}

int f_InitCar_startRank(lua_State *L, tInitCar *s) {
  lua_pushinteger(L, s->startRank);
  return 1;
}

int f_InitCar_driverType(lua_State *L, tInitCar *s) {
  lua_pushinteger(L, s->driverType);
  return 1;
}

int f_InitCar_skillLevel(lua_State *L, tInitCar *s) {
  lua_pushinteger(L, s->skillLevel);
  return 1;
}

int f_InitCar_iconColor(lua_State *L, tInitCar *s) {
  int length = 3;
  tdble *array = s->iconColor;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);
    lua_pushnumber(L, array[i]);
    lua_settable(L, -3);
  }

  return 1;
}

int f_InitCar_dimension(lua_State *L, tInitCar *s) {
  tl_3Dd *wrapper = (tl_3Dd *) lua_newuserdata(L, sizeof(tl_3Dd));
  wrapper->wrapped = &(s->dimension);

  luaL_getmetatable(L, "torcs.3Dd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_InitCar_drvPos(lua_State *L, tInitCar *s) {
  tl_3Dd *wrapper = (tl_3Dd *) lua_newuserdata(L, sizeof(tl_3Dd));
  wrapper->wrapped = &(s->drvPos);

  luaL_getmetatable(L, "torcs.3Dd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_InitCar_bonnetPos(lua_State *L, tInitCar *s) {
  tl_3Dd *wrapper = (tl_3Dd *) lua_newuserdata(L, sizeof(tl_3Dd));
  wrapper->wrapped = &(s->bonnetPos);

  luaL_getmetatable(L, "torcs.3Dd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_InitCar_tank(lua_State *L, tInitCar *s) {
  lua_pushnumber(L, s->tank);
  return 1;
}

int f_InitCar_steerLock(lua_State *L, tInitCar *s) {
  lua_pushnumber(L, s->steerLock);
  return 1;
}

int f_InitCar_statGC(lua_State *L, tInitCar *s) {
  tl_3Dd *wrapper = (tl_3Dd *) lua_newuserdata(L, sizeof(tl_3Dd));
  wrapper->wrapped = &(s->statGC);

  luaL_getmetatable(L, "torcs.3Dd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_InitCar_wheel(lua_State *L, tInitCar *s) {
  int length = 4;
  tWheelSpec *array = s->wheel;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);

    tl_WheelSpec *wrapper = (tl_WheelSpec *) lua_newuserdata(L, sizeof(tl_WheelSpec));
    wrapper->wrapped = &(array[i]);

    luaL_getmetatable(L, "torcs.WheelSpec");
    lua_setmetatable(L, -2);

    lua_settable(L, -3);
  }

  return 1;
}

int f_InitCar_visualAttr(lua_State *L, tInitCar *s) {
  tl_VisualAttributes *wrapper = (tl_VisualAttributes *) lua_newuserdata(L, sizeof(tl_VisualAttributes));
  wrapper->wrapped = &(s->visualAttr);

  luaL_getmetatable(L, "torcs.VisualAttributes");
  lua_setmetatable(L, -2);
  return 1;
}

/** tVisualAttributes **/

int dispatch_VisualAttributes(lua_State *L) {
  tl_VisualAttributes *wrapper = (tl_VisualAttributes *) luaL_checkudata(L, 1, "torcs.VisualAttributes");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_VisualAttributes");

  tVisualAttributes *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_VisualAttributes f = NULL;
  int i = 0;
  while (fields_VisualAttributes[i].name != NULL && strcmp(fields_VisualAttributes[i].name, field) != 0) {
    i++;
  }

  f = fields_VisualAttributes[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_VisualAttributes_exhaustNb(lua_State *L, tVisualAttributes *s) {
  lua_pushinteger(L, s->exhaustNb);
  return 1;
}

int f_VisualAttributes_exhaustPos(lua_State *L, tVisualAttributes *s) {
  int length = 2;
  t3Dd *array = s->exhaustPos;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);

    tl_3Dd *wrapper = (tl_3Dd *) lua_newuserdata(L, sizeof(tl_3Dd));
    wrapper->wrapped = &(array[i]);

    luaL_getmetatable(L, "torcs.3Dd");
    lua_setmetatable(L, -2);

    lua_settable(L, -3);
  }

  return 1;
}

int f_VisualAttributes_exhaustPower(lua_State *L, tVisualAttributes *s) {
  lua_pushnumber(L, s->exhaustPower);
  return 1;
}

/** tPublicCar **/

int dispatch_PublicCar(lua_State *L) {
  tl_PublicCar *wrapper = (tl_PublicCar *) luaL_checkudata(L, 1, "torcs.PublicCar");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_PublicCar");

  tPublicCar *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_PublicCar f = NULL;
  int i = 0;
  while (fields_PublicCar[i].name != NULL && strcmp(fields_PublicCar[i].name, field) != 0) {
    i++;
  }

  f = fields_PublicCar[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_PublicCar_DynGC(lua_State *L, tPublicCar *s) {
  tl_DynPt *wrapper = (tl_DynPt *) lua_newuserdata(L, sizeof(tl_DynPt));
  wrapper->wrapped = &(s->DynGC);

  luaL_getmetatable(L, "torcs.DynPt");
  lua_setmetatable(L, -2);
  return 1;
}

int f_PublicCar_DynGCg(lua_State *L, tPublicCar *s) {
  tl_DynPt *wrapper = (tl_DynPt *) lua_newuserdata(L, sizeof(tl_DynPt));
  wrapper->wrapped = &(s->DynGCg);

  luaL_getmetatable(L, "torcs.DynPt");
  lua_setmetatable(L, -2);
  return 1;
}

int f_PublicCar_speed(lua_State *L, tPublicCar *s) {
  lua_pushnumber(L, s->speed);
  return 1;
}

int f_PublicCar_trkPos(lua_State *L, tPublicCar *s) {
  tl_TrkLocPos *wrapper = (tl_TrkLocPos *) lua_newuserdata(L, sizeof(tl_TrkLocPos));
  wrapper->wrapped = &(s->trkPos);

  luaL_getmetatable(L, "torcs.TrkLocPos");
  lua_setmetatable(L, -2);
  return 1;
}

int f_PublicCar_state(lua_State *L, tPublicCar *s) {
  lua_pushinteger(L, s->state);
  return 1;
}

int f_PublicCar_corner(lua_State *L, tPublicCar *s) {
  int length = 4;
  tPosd *array = s->corner;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);

    tl_Posd *wrapper = (tl_Posd *) lua_newuserdata(L, sizeof(tl_Posd));
    wrapper->wrapped = &(array[i]);

    luaL_getmetatable(L, "torcs.Posd");
    lua_setmetatable(L, -2);

    lua_settable(L, -3);
  }

  return 1;
}

/** tPrivCar **/

int dispatch_PrivCar(lua_State *L) {
  tl_PrivCar *wrapper = (tl_PrivCar *) luaL_checkudata(L, 1, "torcs.PrivCar");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_PrivCar");

  tPrivCar *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_PrivCar f = NULL;
  int i = 0;
  while (fields_PrivCar[i].name != NULL && strcmp(fields_PrivCar[i].name, field) != 0) {
    i++;
  }

  f = fields_PrivCar[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_PrivCar_driverIndex(lua_State *L, tPrivCar *s) {
  lua_pushinteger(L, s->driverIndex);
  return 1;
}

int f_PrivCar_modName(lua_State *L, tPrivCar *s) {
  lua_pushstring(L, s->modName);
  return 1;
}

int f_PrivCar_wheel(lua_State *L, tPrivCar *s) {
  int length = 4;
  tWheelState *array = s->wheel;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);

    tl_WheelState *wrapper = (tl_WheelState *) lua_newuserdata(L, sizeof(tl_WheelState));
    wrapper->wrapped = &(array[i]);

    luaL_getmetatable(L, "torcs.WheelState");
    lua_setmetatable(L, -2);

    lua_settable(L, -3);
  }

  return 1;
}

int f_PrivCar_corner(lua_State *L, tPrivCar *s) {
  int length = 4;
  tPosd *array = s->corner;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);

    tl_Posd *wrapper = (tl_Posd *) lua_newuserdata(L, sizeof(tl_Posd));
    wrapper->wrapped = &(array[i]);

    luaL_getmetatable(L, "torcs.Posd");
    lua_setmetatable(L, -2);

    lua_settable(L, -3);
  }

  return 1;
}

int f_PrivCar_gear(lua_State *L, tPrivCar *s) {
  lua_pushinteger(L, s->gear);
  return 1;
}

int f_PrivCar_fuel(lua_State *L, tPrivCar *s) {
  lua_pushnumber(L, s->fuel);
  return 1;
}

int f_PrivCar_enginerpm(lua_State *L, tPrivCar *s) {
  lua_pushnumber(L, s->enginerpm);
  return 1;
}

int f_PrivCar_enginerpmRedLine(lua_State *L, tPrivCar *s) {
  lua_pushnumber(L, s->enginerpmRedLine);
  return 1;
}

int f_PrivCar_enginerpmMax(lua_State *L, tPrivCar *s) {
  lua_pushnumber(L, s->enginerpmMax);
  return 1;
}

int f_PrivCar_enginerpmMaxTq(lua_State *L, tPrivCar *s) {
  lua_pushnumber(L, s->enginerpmMaxTq);
  return 1;
}

int f_PrivCar_enginerpmMaxPw(lua_State *L, tPrivCar *s) {
  lua_pushnumber(L, s->enginerpmMaxPw);
  return 1;
}

int f_PrivCar_engineMaxTq(lua_State *L, tPrivCar *s) {
  lua_pushnumber(L, s->engineMaxTq);
  return 1;
}

int f_PrivCar_engineMaxPw(lua_State *L, tPrivCar *s) {
  lua_pushnumber(L, s->engineMaxPw);
  return 1;
}

int f_PrivCar_gearRatio(lua_State *L, tPrivCar *s) {
  int length = 10;
  tdble *array = s->gearRatio;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);
    lua_pushnumber(L, array[i]);
    lua_settable(L, -3);
  }

  return 1;
}

int f_PrivCar_gearNb(lua_State *L, tPrivCar *s) {
  lua_pushinteger(L, s->gearNb);
  return 1;
}

int f_PrivCar_gearOffset(lua_State *L, tPrivCar *s) {
  lua_pushinteger(L, s->gearOffset);
  return 1;
}

int f_PrivCar_skid(lua_State *L, tPrivCar *s) {
  int length = 4;
  tdble *array = s->skid;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);
    lua_pushnumber(L, array[i]);
    lua_settable(L, -3);
  }

  return 1;
}

int f_PrivCar_reaction(lua_State *L, tPrivCar *s) {
  int length = 4;
  tdble *array = s->reaction;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);
    lua_pushnumber(L, array[i]);
    lua_settable(L, -3);
  }

  return 1;
}

int f_PrivCar_collision(lua_State *L, tPrivCar *s) {
  lua_pushinteger(L, s->collision);
  return 1;
}

int f_PrivCar_simcollision(lua_State *L, tPrivCar *s) {
  lua_pushinteger(L, s->simcollision);
  return 1;
}

int f_PrivCar_smoke(lua_State *L, tPrivCar *s) {
  lua_pushnumber(L, s->smoke);
  return 1;
}

int f_PrivCar_normal(lua_State *L, tPrivCar *s) {
  tl_3Dd *wrapper = (tl_3Dd *) lua_newuserdata(L, sizeof(tl_3Dd));
  wrapper->wrapped = &(s->normal);

  luaL_getmetatable(L, "torcs.3Dd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_PrivCar_collpos(lua_State *L, tPrivCar *s) {
  tl_3Dd *wrapper = (tl_3Dd *) lua_newuserdata(L, sizeof(tl_3Dd));
  wrapper->wrapped = &(s->collpos);

  luaL_getmetatable(L, "torcs.3Dd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_PrivCar_dammage(lua_State *L, tPrivCar *s) {
  lua_pushinteger(L, s->dammage);
  return 1;
}

int f_PrivCar_debug(lua_State *L, tPrivCar *s) {
  lua_pushinteger(L, s->debug);
  return 1;
}

int f_PrivCar_collision_state(lua_State *L, tPrivCar *s) {
  tl_CollisionState *wrapper = (tl_CollisionState *) lua_newuserdata(L, sizeof(tl_CollisionState));
  wrapper->wrapped = &(s->collision_state);

  luaL_getmetatable(L, "torcs.CollisionState");
  lua_setmetatable(L, -2);
  return 1;
}

/** tCollisionState **/

int dispatch_CollisionState(lua_State *L) {
  tl_CollisionState *wrapper = (tl_CollisionState *) luaL_checkudata(L, 1, "torcs.CollisionState");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_CollisionState");

  tCollisionState *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_CollisionState f = NULL;
  int i = 0;
  while (fields_CollisionState[i].name != NULL && strcmp(fields_CollisionState[i].name, field) != 0) {
    i++;
  }

  f = fields_CollisionState[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_CollisionState_collision_count(lua_State *L, tCollisionState *s) {
  lua_pushinteger(L, s->collision_count);
  return 1;
}

int f_CollisionState_pos(lua_State *L, tCollisionState *s) {
  int length = 3;
  tdble *array = s->pos;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);
    lua_pushnumber(L, array[i]);
    lua_settable(L, -3);
  }

  return 1;
}

int f_CollisionState_force(lua_State *L, tCollisionState *s) {
  int length = 3;
  tdble *array = s->force;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);
    lua_pushnumber(L, array[i]);
    lua_settable(L, -3);
  }

  return 1;
}

/** tWheelSpec **/

int dispatch_WheelSpec(lua_State *L) {
  tl_WheelSpec *wrapper = (tl_WheelSpec *) luaL_checkudata(L, 1, "torcs.WheelSpec");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_WheelSpec");

  tWheelSpec *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_WheelSpec f = NULL;
  int i = 0;
  while (fields_WheelSpec[i].name != NULL && strcmp(fields_WheelSpec[i].name, field) != 0) {
    i++;
  }

  f = fields_WheelSpec[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_WheelSpec_rimRadius(lua_State *L, tWheelSpec *s) {
  lua_pushnumber(L, s->rimRadius);
  return 1;
}

int f_WheelSpec_tireHeight(lua_State *L, tWheelSpec *s) {
  lua_pushnumber(L, s->tireHeight);
  return 1;
}

int f_WheelSpec_tireWidth(lua_State *L, tWheelSpec *s) {
  lua_pushnumber(L, s->tireWidth);
  return 1;
}

int f_WheelSpec_brakeDiskRadius(lua_State *L, tWheelSpec *s) {
  lua_pushnumber(L, s->brakeDiskRadius);
  return 1;
}

int f_WheelSpec_wheelRadius(lua_State *L, tWheelSpec *s) {
  lua_pushnumber(L, s->wheelRadius);
  return 1;
}

/** tWheelState **/

int dispatch_WheelState(lua_State *L) {
  tl_WheelState *wrapper = (tl_WheelState *) luaL_checkudata(L, 1, "torcs.WheelState");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_WheelState");

  tWheelState *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_WheelState f = NULL;
  int i = 0;
  while (fields_WheelState[i].name != NULL && strcmp(fields_WheelState[i].name, field) != 0) {
    i++;
  }

  f = fields_WheelState[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_WheelState_relPos(lua_State *L, tWheelState *s) {
  tl_Posd *wrapper = (tl_Posd *) lua_newuserdata(L, sizeof(tl_Posd));
  wrapper->wrapped = &(s->relPos);

  luaL_getmetatable(L, "torcs.Posd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_WheelState_spinVel(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->spinVel);
  return 1;
}

int f_WheelState_brakeTemp(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->brakeTemp);
  return 1;
}

int f_WheelState_state(lua_State *L, tWheelState *s) {
  lua_pushinteger(L, s->state);
  return 1;
}

int f_WheelState_seg(lua_State *L, tWheelState *s) {
  tl_TrackSeg *wrapper = (tl_TrackSeg *) lua_newuserdata(L, sizeof(tl_TrackSeg));
  wrapper->wrapped = s->seg;

  luaL_getmetatable(L, "torcs.TrackSeg");
  lua_setmetatable(L, -2);
  return 1;
}

int f_WheelState_rollRes(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->rollRes);
  return 1;
}

int f_WheelState_temp_in (lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->temp_in );
  return 1;
}

int f_WheelState_temp_mid(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->temp_mid);
  return 1;
}

int f_WheelState_temp_out(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->temp_out);
  return 1;
}

int f_WheelState_condition(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->condition);
  return 1;
}

int f_WheelState_slipSide(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->slipSide);
  return 1;
}

int f_WheelState_slipAccel(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->slipAccel);
  return 1;
}

int f_WheelState_Fx(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->Fx);
  return 1;
}

int f_WheelState_Fy(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->Fy);
  return 1;
}

int f_WheelState_Fz(lua_State *L, tWheelState *s) {
  lua_pushnumber(L, s->Fz);
  return 1;
}

/** tTrkLocPos **/

int dispatch_TrkLocPos(lua_State *L) {
  tl_TrkLocPos *wrapper = (tl_TrkLocPos *) luaL_checkudata(L, 1, "torcs.TrkLocPos");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_TrkLocPos");

  tTrkLocPos *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_TrkLocPos f = NULL;
  int i = 0;
  while (fields_TrkLocPos[i].name != NULL && strcmp(fields_TrkLocPos[i].name, field) != 0) {
    i++;
  }

  f = fields_TrkLocPos[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_TrkLocPos_seg(lua_State *L, tTrkLocPos *s) {
  tl_TrackSeg *wrapper = (tl_TrackSeg *) lua_newuserdata(L, sizeof(tl_TrackSeg));
  wrapper->wrapped = s->seg;

  luaL_getmetatable(L, "torcs.TrackSeg");
  lua_setmetatable(L, -2);
  return 1;
}

int f_TrkLocPos_type(lua_State *L, tTrkLocPos *s) {
  lua_pushinteger(L, s->type);
  return 1;
}

int f_TrkLocPos_toStart(lua_State *L, tTrkLocPos *s) {
  lua_pushnumber(L, s->toStart);
  return 1;
}

int f_TrkLocPos_toRight(lua_State *L, tTrkLocPos *s) {
  lua_pushnumber(L, s->toRight);
  return 1;
}

int f_TrkLocPos_toMiddle(lua_State *L, tTrkLocPos *s) {
  lua_pushnumber(L, s->toMiddle);
  return 1;
}

int f_TrkLocPos_toLeft(lua_State *L, tTrkLocPos *s) {
  lua_pushnumber(L, s->toLeft);
  return 1;
}

/** tTrackSeg **/

int dispatch_TrackSeg(lua_State *L) {
  tl_TrackSeg *wrapper = (tl_TrackSeg *) luaL_checkudata(L, 1, "torcs.TrackSeg");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_TrackSeg");

  tTrackSeg *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_TrackSeg f = NULL;
  int i = 0;
  while (fields_TrackSeg[i].name != NULL && strcmp(fields_TrackSeg[i].name, field) != 0) {
    i++;
  }

  f = fields_TrackSeg[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_TrackSeg_name(lua_State *L, tTrackSeg *s) {
  lua_pushstring(L, s->name);
  return 1;
}

int f_TrackSeg_id(lua_State *L, tTrackSeg *s) {
  lua_pushinteger(L, s->id);
  return 1;
}

int f_TrackSeg_type(lua_State *L, tTrackSeg *s) {
  lua_pushinteger(L, s->type);
  return 1;
}

int f_TrackSeg_type2(lua_State *L, tTrackSeg *s) {
  lua_pushinteger(L, s->type2);
  return 1;
}

int f_TrackSeg_style(lua_State *L, tTrackSeg *s) {
  lua_pushinteger(L, s->style);
  return 1;
}

int f_TrackSeg_length(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->length);
  return 1;
}

int f_TrackSeg_width(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->width);
  return 1;
}

int f_TrackSeg_startWidth(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->startWidth);
  return 1;
}

int f_TrackSeg_endWidth(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->endWidth);
  return 1;
}

int f_TrackSeg_lgfromstart(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->lgfromstart);
  return 1;
}

int f_TrackSeg_radius(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->radius);
  return 1;
}

int f_TrackSeg_radiusr(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->radiusr);
  return 1;
}

int f_TrackSeg_radiusl(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->radiusl);
  return 1;
}

int f_TrackSeg_arc(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->arc);
  return 1;
}

int f_TrackSeg_center(lua_State *L, tTrackSeg *s) {
  tl_3Dd *wrapper = (tl_3Dd *) lua_newuserdata(L, sizeof(tl_3Dd));
  wrapper->wrapped = &(s->center);

  luaL_getmetatable(L, "torcs.3Dd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_TrackSeg_vertex(lua_State *L, tTrackSeg *s) {
  int length = 4;
  t3Dd *array = s->vertex;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);

    tl_3Dd *wrapper = (tl_3Dd *) lua_newuserdata(L, sizeof(tl_3Dd));
    wrapper->wrapped = &(array[i]);

    luaL_getmetatable(L, "torcs.3Dd");
    lua_setmetatable(L, -2);

    lua_settable(L, -3);
  }

  return 1;
}

int f_TrackSeg_angle(lua_State *L, tTrackSeg *s) {
  int length = 7;
  tdble *array = s->angle;

  lua_createtable(L, length, 0);

  int i;
  for (i = 0; i < length; i++) {
    lua_pushinteger(L, i + 1);
    lua_pushnumber(L, array[i]);
    lua_settable(L, -3);
  }

  return 1;
}

int f_TrackSeg_Kzl(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->Kzl);
  return 1;
}

int f_TrackSeg_Kzw(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->Kzw);
  return 1;
}

int f_TrackSeg_Kyl(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->Kyl);
  return 1;
}

int f_TrackSeg_rgtSideNormal(lua_State *L, tTrackSeg *s) {
  tl_3Dd *wrapper = (tl_3Dd *) lua_newuserdata(L, sizeof(tl_3Dd));
  wrapper->wrapped = &(s->rgtSideNormal);

  luaL_getmetatable(L, "torcs.3Dd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_TrackSeg_envIndex(lua_State *L, tTrackSeg *s) {
  lua_pushinteger(L, s->envIndex);
  return 1;
}

int f_TrackSeg_height(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->height);
  return 1;
}

int f_TrackSeg_raceInfo(lua_State *L, tTrackSeg *s) {
  lua_pushinteger(L, s->raceInfo);
  return 1;
}

int f_TrackSeg_DoVfactor(lua_State *L, tTrackSeg *s) {
  lua_pushnumber(L, s->DoVfactor);
  return 1;
}

int f_TrackSeg_next(lua_State *L, tTrackSeg *s) {
  tl_TrackSeg *wrapper = (tl_TrackSeg *) lua_newuserdata(L, sizeof(tl_TrackSeg));
  wrapper->wrapped = s->next;

  luaL_getmetatable(L, "torcs.TrackSeg");
  lua_setmetatable(L, -2);
  return 1;
}

int f_TrackSeg_prev(lua_State *L, tTrackSeg *s) {
  tl_TrackSeg *wrapper = (tl_TrackSeg *) lua_newuserdata(L, sizeof(tl_TrackSeg));
  wrapper->wrapped = s->prev;

  luaL_getmetatable(L, "torcs.TrackSeg");
  lua_setmetatable(L, -2);
  return 1;
}

/** t3Dd **/

int dispatch_3Dd(lua_State *L) {
  tl_3Dd *wrapper = (tl_3Dd *) luaL_checkudata(L, 1, "torcs.3Dd");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_3Dd");

  t3Dd *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_3Dd f = NULL;
  int i = 0;
  while (fields_3Dd[i].name != NULL && strcmp(fields_3Dd[i].name, field) != 0) {
    i++;
  }

  f = fields_3Dd[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_3Dd_x(lua_State *L, t3Dd *s) {
  lua_pushnumber(L, s->x);
  return 1;
}

int f_3Dd_y(lua_State *L, t3Dd *s) {
  lua_pushnumber(L, s->y);
  return 1;
}

int f_3Dd_z(lua_State *L, t3Dd *s) {
  lua_pushnumber(L, s->z);
  return 1;
}

/** tPosd **/

int dispatch_Posd(lua_State *L) {
  tl_Posd *wrapper = (tl_Posd *) luaL_checkudata(L, 1, "torcs.Posd");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_Posd");

  tPosd *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_Posd f = NULL;
  int i = 0;
  while (fields_Posd[i].name != NULL && strcmp(fields_Posd[i].name, field) != 0) {
    i++;
  }

  f = fields_Posd[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_Posd_x(lua_State *L, tPosd *s) {
  lua_pushnumber(L, s->x);
  return 1;
}

int f_Posd_y(lua_State *L, tPosd *s) {
  lua_pushnumber(L, s->y);
  return 1;
}

int f_Posd_z(lua_State *L, tPosd *s) {
  lua_pushnumber(L, s->z);
  return 1;
}

int f_Posd_ax(lua_State *L, tPosd *s) {
  lua_pushnumber(L, s->ax);
  return 1;
}

int f_Posd_ay(lua_State *L, tPosd *s) {
  lua_pushnumber(L, s->ay);
  return 1;
}

int f_Posd_az(lua_State *L, tPosd *s) {
  lua_pushnumber(L, s->az);
  return 1;
}

/** tDynPt **/

int dispatch_DynPt(lua_State *L) {
  tl_DynPt *wrapper = (tl_DynPt *) luaL_checkudata(L, 1, "torcs.DynPt");
  luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_DynPt");

  tDynPt *wrapped = wrapper->wrapped;
  const char *field = luaL_checkstring(L, 2);

  getter_DynPt f = NULL;
  int i = 0;
  while (fields_DynPt[i].name != NULL && strcmp(fields_DynPt[i].name, field) != 0) {
    i++;
  }

  f = fields_DynPt[i].getter;
  if (f != NULL) {
    return f(L, wrapped);
  } else {
    missingFieldError(L, field); // Doesn't return
    return -1;
  }
}

int f_DynPt_pos(lua_State *L, tDynPt *s) {
  tl_Posd *wrapper = (tl_Posd *) lua_newuserdata(L, sizeof(tl_Posd));
  wrapper->wrapped = &(s->pos);

  luaL_getmetatable(L, "torcs.Posd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_DynPt_vel(lua_State *L, tDynPt *s) {
  tl_Posd *wrapper = (tl_Posd *) lua_newuserdata(L, sizeof(tl_Posd));
  wrapper->wrapped = &(s->vel);

  luaL_getmetatable(L, "torcs.Posd");
  lua_setmetatable(L, -2);
  return 1;
}

int f_DynPt_acc(lua_State *L, tDynPt *s) {
  tl_Posd *wrapper = (tl_Posd *) lua_newuserdata(L, sizeof(tl_Posd));
  wrapper->wrapped = &(s->acc);

  luaL_getmetatable(L, "torcs.Posd");
  lua_setmetatable(L, -2);
  return 1;
}

