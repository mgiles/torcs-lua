#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <car.h>
#include <robottools.h>

/** top-level functions **/

int tl_RtTrackSideTgAngleL(lua_State *L);

const luaL_Reg tl_functions[] = {
  {"RtTrackSideTgAngleL", tl_RtTrackSideTgAngleL},
  {NULL, NULL}
};

/** Struct dispatch **/

int dispatch_CarElt(lua_State *L);
int dispatch_InitCar(lua_State *L);
int dispatch_VisualAttributes(lua_State *L);
int dispatch_PublicCar(lua_State *L);
int dispatch_PrivCar(lua_State *L);
int dispatch_CollisionState(lua_State *L);
int dispatch_WheelSpec(lua_State *L);
int dispatch_WheelState(lua_State *L);
int dispatch_TrkLocPos(lua_State *L);
int dispatch_TrackSeg(lua_State *L);
int dispatch_3Dd(lua_State *L);
int dispatch_Posd(lua_State *L);
int dispatch_DynPt(lua_State *L);
int dispatch_TrackSurface(lua_State *L);

const luaL_Reg dispatchers[] = {
  {"torcs.CarElt", dispatch_CarElt},
  {"torcs.InitCar", dispatch_InitCar},
  {"torcs.VisualAttributes", dispatch_VisualAttributes},
  {"torcs.PublicCar", dispatch_PublicCar},
  {"torcs.PrivCar", dispatch_PrivCar},
  {"torcs.CollisionState", dispatch_CollisionState},
  {"torcs.WheelSpec", dispatch_WheelSpec},
  {"torcs.WheelState", dispatch_WheelState},
  {"torcs.TrkLocPos", dispatch_TrkLocPos},
  {"torcs.TrackSeg", dispatch_TrackSeg},
  {"torcs.3Dd", dispatch_3Dd},
  {"torcs.Posd", dispatch_Posd},
  {"torcs.DynPt", dispatch_DynPt},
  {"torcs.TrackSurface", dispatch_TrackSurface},
  {NULL, NULL}
};

/** tCarElt **/

typedef struct {
  tCarElt *wrapped;
} tl_CarElt;

int f_CarElt_index(lua_State *L, tCarElt *wrapped);
int f_CarElt_info(lua_State *L, tCarElt *wrapped);
int f_CarElt_pub(lua_State *L, tCarElt *wrapped);
int f_CarElt_priv(lua_State *L, tCarElt *wrapped);

typedef int (*getter_CarElt) (lua_State *L, tCarElt *wrapped);

typedef struct {
  const char *name;
  getter_CarElt getter;
} getterEntry_CarElt;

const getterEntry_CarElt fields_CarElt[] = {
  {"index", f_CarElt_index},
  {"info", f_CarElt_info},
  {"pub", f_CarElt_pub},
  {"priv", f_CarElt_priv},
  {NULL, NULL}
};

/** tInitCar **/

typedef struct {
  tInitCar *wrapped;
} tl_InitCar;

int f_InitCar_name(lua_State *L, tInitCar *wrapped);
int f_InitCar_teamname(lua_State *L, tInitCar *wrapped);
int f_InitCar_carName(lua_State *L, tInitCar *wrapped);
int f_InitCar_category(lua_State *L, tInitCar *wrapped);
int f_InitCar_raceNumber(lua_State *L, tInitCar *wrapped);
int f_InitCar_startRank(lua_State *L, tInitCar *wrapped);
int f_InitCar_driverType(lua_State *L, tInitCar *wrapped);
int f_InitCar_skillLevel(lua_State *L, tInitCar *wrapped);
int f_InitCar_iconColor(lua_State *L, tInitCar *wrapped);
int f_InitCar_dimension(lua_State *L, tInitCar *wrapped);
int f_InitCar_drvPos(lua_State *L, tInitCar *wrapped);
int f_InitCar_bonnetPos(lua_State *L, tInitCar *wrapped);
int f_InitCar_tank(lua_State *L, tInitCar *wrapped);
int f_InitCar_steerLock(lua_State *L, tInitCar *wrapped);
int f_InitCar_statGC(lua_State *L, tInitCar *wrapped);
int f_InitCar_wheel(lua_State *L, tInitCar *wrapped);
int f_InitCar_visualAttr(lua_State *L, tInitCar *wrapped);

typedef int (*getter_InitCar) (lua_State *L, tInitCar *wrapped);

typedef struct {
  const char *name;
  getter_InitCar getter;
} getterEntry_InitCar;

const getterEntry_InitCar fields_InitCar[] = {
  {"name", f_InitCar_name},
  {"teamname", f_InitCar_teamname},
  {"carName", f_InitCar_carName},
  {"category", f_InitCar_category},
  {"raceNumber", f_InitCar_raceNumber},
  {"startRank", f_InitCar_startRank},
  {"driverType", f_InitCar_driverType},
  {"skillLevel", f_InitCar_skillLevel},
  {"iconColor", f_InitCar_iconColor},
  {"dimension", f_InitCar_dimension},
  {"drvPos", f_InitCar_drvPos},
  {"bonnetPos", f_InitCar_bonnetPos},
  {"tank", f_InitCar_tank},
  {"steerLock", f_InitCar_steerLock},
  {"statGC", f_InitCar_statGC},
  {"wheel", f_InitCar_wheel},
  {"visualAttr", f_InitCar_visualAttr},
  {NULL, NULL}
};

/** tVisualAttributes **/

typedef struct {
  tVisualAttributes *wrapped;
} tl_VisualAttributes;

int f_VisualAttributes_exhaustNb(lua_State *L, tVisualAttributes *wrapped);
int f_VisualAttributes_exhaustPos(lua_State *L, tVisualAttributes *wrapped);
int f_VisualAttributes_exhaustPower(lua_State *L, tVisualAttributes *wrapped);

typedef int (*getter_VisualAttributes) (lua_State *L, tVisualAttributes *wrapped);

typedef struct {
  const char *name;
  getter_VisualAttributes getter;
} getterEntry_VisualAttributes;

const getterEntry_VisualAttributes fields_VisualAttributes[] = {
  {"exhaustNb", f_VisualAttributes_exhaustNb},
  {"exhaustPos", f_VisualAttributes_exhaustPos},
  {"exhaustPower", f_VisualAttributes_exhaustPower},
  {NULL, NULL}
};

/** tPublicCar **/

typedef struct {
  tPublicCar *wrapped;
} tl_PublicCar;

int f_PublicCar_DynGC(lua_State *L, tPublicCar *wrapped);
int f_PublicCar_DynGCg(lua_State *L, tPublicCar *wrapped);
int f_PublicCar_speed(lua_State *L, tPublicCar *wrapped);
int f_PublicCar_trkPos(lua_State *L, tPublicCar *wrapped);
int f_PublicCar_state(lua_State *L, tPublicCar *wrapped);
int f_PublicCar_corner(lua_State *L, tPublicCar *wrapped);

typedef int (*getter_PublicCar) (lua_State *L, tPublicCar *wrapped);

typedef struct {
  const char *name;
  getter_PublicCar getter;
} getterEntry_PublicCar;

const getterEntry_PublicCar fields_PublicCar[] = {
  {"DynGC", f_PublicCar_DynGC},
  {"DynGCg", f_PublicCar_DynGCg},
  {"speed", f_PublicCar_speed},
  {"trkPos", f_PublicCar_trkPos},
  {"state", f_PublicCar_state},
  {"corner", f_PublicCar_corner},
  {NULL, NULL}
};

/** tPrivCar **/

typedef struct {
  tPrivCar *wrapped;
} tl_PrivCar;

int f_PrivCar_driverIndex(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_modName(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_wheel(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_corner(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_gear(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_fuel(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_enginerpm(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_enginerpmRedLine(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_enginerpmMax(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_enginerpmMaxTq(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_enginerpmMaxPw(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_engineMaxTq(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_engineMaxPw(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_gearRatio(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_gearNb(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_gearOffset(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_skid(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_reaction(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_collision(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_simcollision(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_smoke(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_normal(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_collpos(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_dammage(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_debug(lua_State *L, tPrivCar *wrapped);
int f_PrivCar_collision_state(lua_State *L, tPrivCar *wrapped);

typedef int (*getter_PrivCar) (lua_State *L, tPrivCar *wrapped);

typedef struct {
  const char *name;
  getter_PrivCar getter;
} getterEntry_PrivCar;

const getterEntry_PrivCar fields_PrivCar[] = {
  {"driverIndex", f_PrivCar_driverIndex},
  {"modName", f_PrivCar_modName},
  {"wheel", f_PrivCar_wheel},
  {"corner", f_PrivCar_corner},
  {"gear", f_PrivCar_gear},
  {"fuel", f_PrivCar_fuel},
  {"enginerpm", f_PrivCar_enginerpm},
  {"enginerpmRedLine", f_PrivCar_enginerpmRedLine},
  {"enginerpmMax", f_PrivCar_enginerpmMax},
  {"enginerpmMaxTq", f_PrivCar_enginerpmMaxTq},
  {"enginerpmMaxPw", f_PrivCar_enginerpmMaxPw},
  {"engineMaxTq", f_PrivCar_engineMaxTq},
  {"engineMaxPw", f_PrivCar_engineMaxPw},
  {"gearRatio", f_PrivCar_gearRatio},
  {"gearNb", f_PrivCar_gearNb},
  {"gearOffset", f_PrivCar_gearOffset},
  {"skid", f_PrivCar_skid},
  {"reaction", f_PrivCar_reaction},
  {"collision", f_PrivCar_collision},
  {"simcollision", f_PrivCar_simcollision},
  {"smoke", f_PrivCar_smoke},
  {"normal", f_PrivCar_normal},
  {"collpos", f_PrivCar_collpos},
  {"dammage", f_PrivCar_dammage},
  {"debug", f_PrivCar_debug},
  {"collision_state", f_PrivCar_collision_state},
  {NULL, NULL}
};

/** tCollisionState **/

typedef struct {
  tCollisionState *wrapped;
} tl_CollisionState;

int f_CollisionState_collision_count(lua_State *L, tCollisionState *wrapped);
int f_CollisionState_pos(lua_State *L, tCollisionState *wrapped);
int f_CollisionState_force(lua_State *L, tCollisionState *wrapped);

typedef int (*getter_CollisionState) (lua_State *L, tCollisionState *wrapped);

typedef struct {
  const char *name;
  getter_CollisionState getter;
} getterEntry_CollisionState;

const getterEntry_CollisionState fields_CollisionState[] = {
  {"collision_count", f_CollisionState_collision_count},
  {"pos", f_CollisionState_pos},
  {"force", f_CollisionState_force},
  {NULL, NULL}
};

/** tWheelSpec **/

typedef struct {
  tWheelSpec *wrapped;
} tl_WheelSpec;

int f_WheelSpec_rimRadius(lua_State *L, tWheelSpec *wrapped);
int f_WheelSpec_tireHeight(lua_State *L, tWheelSpec *wrapped);
int f_WheelSpec_tireWidth(lua_State *L, tWheelSpec *wrapped);
int f_WheelSpec_brakeDiskRadius(lua_State *L, tWheelSpec *wrapped);
int f_WheelSpec_wheelRadius(lua_State *L, tWheelSpec *wrapped);

typedef int (*getter_WheelSpec) (lua_State *L, tWheelSpec *wrapped);

typedef struct {
  const char *name;
  getter_WheelSpec getter;
} getterEntry_WheelSpec;

const getterEntry_WheelSpec fields_WheelSpec[] = {
  {"rimRadius", f_WheelSpec_rimRadius},
  {"tireHeight", f_WheelSpec_tireHeight},
  {"tireWidth", f_WheelSpec_tireWidth},
  {"brakeDiskRadius", f_WheelSpec_brakeDiskRadius},
  {"wheelRadius", f_WheelSpec_wheelRadius},
  {NULL, NULL}
};

/** tWheelState **/

typedef struct {
  tWheelState *wrapped;
} tl_WheelState;

int f_WheelState_relPos(lua_State *L, tWheelState *wrapped);
int f_WheelState_spinVel(lua_State *L, tWheelState *wrapped);
int f_WheelState_brakeTemp(lua_State *L, tWheelState *wrapped);
int f_WheelState_state(lua_State *L, tWheelState *wrapped);
int f_WheelState_seg(lua_State *L, tWheelState *wrapped);
int f_WheelState_rollRes(lua_State *L, tWheelState *wrapped);
int f_WheelState_temp_in (lua_State *L, tWheelState *wrapped);
int f_WheelState_temp_mid(lua_State *L, tWheelState *wrapped);
int f_WheelState_temp_out(lua_State *L, tWheelState *wrapped);
int f_WheelState_condition(lua_State *L, tWheelState *wrapped);
int f_WheelState_slipSide(lua_State *L, tWheelState *wrapped);
int f_WheelState_slipAccel(lua_State *L, tWheelState *wrapped);
int f_WheelState_Fx(lua_State *L, tWheelState *wrapped);
int f_WheelState_Fy(lua_State *L, tWheelState *wrapped);
int f_WheelState_Fz(lua_State *L, tWheelState *wrapped);

typedef int (*getter_WheelState) (lua_State *L, tWheelState *wrapped);

typedef struct {
  const char *name;
  getter_WheelState getter;
} getterEntry_WheelState;

const getterEntry_WheelState fields_WheelState[] = {
  {"relPos", f_WheelState_relPos},
  {"spinVel", f_WheelState_spinVel},
  {"brakeTemp", f_WheelState_brakeTemp},
  {"state", f_WheelState_state},
  {"seg", f_WheelState_seg},
  {"rollRes", f_WheelState_rollRes},
  {"temp_in ", f_WheelState_temp_in },
  {"temp_mid", f_WheelState_temp_mid},
  {"temp_out", f_WheelState_temp_out},
  {"condition", f_WheelState_condition},
  {"slipSide", f_WheelState_slipSide},
  {"slipAccel", f_WheelState_slipAccel},
  {"Fx", f_WheelState_Fx},
  {"Fy", f_WheelState_Fy},
  {"Fz", f_WheelState_Fz},
  {NULL, NULL}
};

/** tTrkLocPos **/

typedef struct {
  tTrkLocPos *wrapped;
} tl_TrkLocPos;

int f_TrkLocPos_seg(lua_State *L, tTrkLocPos *wrapped);
int f_TrkLocPos_type(lua_State *L, tTrkLocPos *wrapped);
int f_TrkLocPos_toStart(lua_State *L, tTrkLocPos *wrapped);
int f_TrkLocPos_toRight(lua_State *L, tTrkLocPos *wrapped);
int f_TrkLocPos_toMiddle(lua_State *L, tTrkLocPos *wrapped);
int f_TrkLocPos_toLeft(lua_State *L, tTrkLocPos *wrapped);

typedef int (*getter_TrkLocPos) (lua_State *L, tTrkLocPos *wrapped);

typedef struct {
  const char *name;
  getter_TrkLocPos getter;
} getterEntry_TrkLocPos;

const getterEntry_TrkLocPos fields_TrkLocPos[] = {
  {"seg", f_TrkLocPos_seg},
  {"type", f_TrkLocPos_type},
  {"toStart", f_TrkLocPos_toStart},
  {"toRight", f_TrkLocPos_toRight},
  {"toMiddle", f_TrkLocPos_toMiddle},
  {"toLeft", f_TrkLocPos_toLeft},
  {NULL, NULL}
};

/** tTrackSeg **/

typedef struct {
  tTrackSeg *wrapped;
} tl_TrackSeg;

int f_TrackSeg_name(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_id(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_type(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_type2(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_style(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_length(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_width(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_startWidth(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_endWidth(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_lgfromstart(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_radius(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_radiusr(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_radiusl(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_arc(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_center(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_vertex(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_angle(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_Kzl(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_Kzw(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_Kyl(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_rgtSideNormal(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_envIndex(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_height(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_raceInfo(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_DoVfactor(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_surface(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_next(lua_State *L, tTrackSeg *wrapped);
int f_TrackSeg_prev(lua_State *L, tTrackSeg *wrapped);

typedef int (*getter_TrackSeg) (lua_State *L, tTrackSeg *wrapped);

typedef struct {
  const char *name;
  getter_TrackSeg getter;
} getterEntry_TrackSeg;

const getterEntry_TrackSeg fields_TrackSeg[] = {
  {"name", f_TrackSeg_name},
  {"id", f_TrackSeg_id},
  {"type", f_TrackSeg_type},
  {"type2", f_TrackSeg_type2},
  {"style", f_TrackSeg_style},
  {"length", f_TrackSeg_length},
  {"width", f_TrackSeg_width},
  {"startWidth", f_TrackSeg_startWidth},
  {"endWidth", f_TrackSeg_endWidth},
  {"lgfromstart", f_TrackSeg_lgfromstart},
  {"radius", f_TrackSeg_radius},
  {"radiusr", f_TrackSeg_radiusr},
  {"radiusl", f_TrackSeg_radiusl},
  {"arc", f_TrackSeg_arc},
  {"center", f_TrackSeg_center},
  {"vertex", f_TrackSeg_vertex},
  {"angle", f_TrackSeg_angle},
  {"Kzl", f_TrackSeg_Kzl},
  {"Kzw", f_TrackSeg_Kzw},
  {"Kyl", f_TrackSeg_Kyl},
  {"rgtSideNormal", f_TrackSeg_rgtSideNormal},
  {"envIndex", f_TrackSeg_envIndex},
  {"height", f_TrackSeg_height},
  {"raceInfo", f_TrackSeg_raceInfo},
  {"DoVfactor", f_TrackSeg_DoVfactor},
  {"surface", f_TrackSeg_surface},
  {"next", f_TrackSeg_next},
  {"prev", f_TrackSeg_prev},
  {NULL, NULL}
};

/** t3Dd **/

typedef struct {
  t3Dd *wrapped;
} tl_3Dd;

int f_3Dd_x(lua_State *L, t3Dd *wrapped);
int f_3Dd_y(lua_State *L, t3Dd *wrapped);
int f_3Dd_z(lua_State *L, t3Dd *wrapped);

typedef int (*getter_3Dd) (lua_State *L, t3Dd *wrapped);

typedef struct {
  const char *name;
  getter_3Dd getter;
} getterEntry_3Dd;

const getterEntry_3Dd fields_3Dd[] = {
  {"x", f_3Dd_x},
  {"y", f_3Dd_y},
  {"z", f_3Dd_z},
  {NULL, NULL}
};

/** tPosd **/

typedef struct {
  tPosd *wrapped;
} tl_Posd;

int f_Posd_x(lua_State *L, tPosd *wrapped);
int f_Posd_y(lua_State *L, tPosd *wrapped);
int f_Posd_z(lua_State *L, tPosd *wrapped);
int f_Posd_ax(lua_State *L, tPosd *wrapped);
int f_Posd_ay(lua_State *L, tPosd *wrapped);
int f_Posd_az(lua_State *L, tPosd *wrapped);

typedef int (*getter_Posd) (lua_State *L, tPosd *wrapped);

typedef struct {
  const char *name;
  getter_Posd getter;
} getterEntry_Posd;

const getterEntry_Posd fields_Posd[] = {
  {"x", f_Posd_x},
  {"y", f_Posd_y},
  {"z", f_Posd_z},
  {"ax", f_Posd_ax},
  {"ay", f_Posd_ay},
  {"az", f_Posd_az},
  {NULL, NULL}
};

/** tDynPt **/

typedef struct {
  tDynPt *wrapped;
} tl_DynPt;

int f_DynPt_pos(lua_State *L, tDynPt *wrapped);
int f_DynPt_vel(lua_State *L, tDynPt *wrapped);
int f_DynPt_acc(lua_State *L, tDynPt *wrapped);

typedef int (*getter_DynPt) (lua_State *L, tDynPt *wrapped);

typedef struct {
  const char *name;
  getter_DynPt getter;
} getterEntry_DynPt;

const getterEntry_DynPt fields_DynPt[] = {
  {"pos", f_DynPt_pos},
  {"vel", f_DynPt_vel},
  {"acc", f_DynPt_acc},
  {NULL, NULL}
};

/** tTrackSurface **/

typedef struct {
  tTrackSurface *wrapped;
} tl_TrackSurface;

int f_TrackSurface_next(lua_State *L, tTrackSurface *wrapped);
int f_TrackSurface_material(lua_State *L, tTrackSurface *wrapped);
int f_TrackSurface_kFriction(lua_State *L, tTrackSurface *wrapped);
int f_TrackSurface_kRebound(lua_State *L, tTrackSurface *wrapped);
int f_TrackSurface_kRollRes(lua_State *L, tTrackSurface *wrapped);
int f_TrackSurface_kRoughness(lua_State *L, tTrackSurface *wrapped);
int f_TrackSurface_kDammage(lua_State *L, tTrackSurface *wrapped);

typedef int (*getter_TrackSurface) (lua_State *L, tTrackSurface *wrapped);

typedef struct {
  const char *name;
  getter_TrackSurface getter;
} getterEntry_TrackSurface;

const getterEntry_TrackSurface fields_TrackSurface[] = {
  {"next", f_TrackSurface_next},
  {"material", f_TrackSurface_material},
  {"kFriction", f_TrackSurface_kFriction},
  {"kRebound", f_TrackSurface_kRebound},
  {"kRollRes", f_TrackSurface_kRollRes},
  {"kRoughness", f_TrackSurface_kRoughness},
  {"kDammage", f_TrackSurface_kDammage},
  {NULL, NULL}
};

