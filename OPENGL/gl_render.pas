//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2021 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_render;

interface

uses
  Windows,
  d_delphi,
  dglOpenGL,
  d_player,
  gl_defs,
  r_defs,
  r_visplanes,
  m_fixed;

var
  extra_red: float = 0.0;
  extra_green: float = 0.0;
  extra_blue: float = 0.0;
  extra_alpha: float = 0.0;

procedure gld_DrawBackground(const name: string);

procedure gld_DrawBackgroundFrame;

procedure gld_Finish;

procedure gld_AddSprite(vspr: Pvissprite_t);

procedure gld_AddWall(seg: Pseg_t{$IFDEF HEXEN}; const ispolyobj: boolean; const ssec: Psector_t{$ENDIF});

procedure gld_StartDrawScene;

procedure gld_DrawScene(player: Pplayer_t);

procedure gld_EndDrawScene;

procedure gld_AddPlane(subsectornum: integer; floor, ceiling: Pvisplane_t);

procedure gld_PreprocessLevel;

procedure gld_DrawWeapon(weaponlump: integer; vis: Pvissprite_t; lightlevel: integer);

procedure gld_Init(width, height: integer);

procedure gld_SetPalette(palette: integer);

procedure gld_DrawNumPatch(x, y: integer; lump: integer; cm: integer; flags: integer;
  const zoomx: float = 1.0; const zoomy: float = 1.0);

procedure gld_Enable2D;

procedure gld_Disable2D;

procedure gld_StaticLight(light: float);

procedure gld_CleanMemory;

procedure R_ShutDownOpenGL;

procedure gld_DrawLine(x0, y0, x1, y1: integer; PalColor: byte);

implementation

uses
  doomstat,
  d_net,
  d_main,
  i_system,
  tables,
  doomtype,
  doomdef,
  am_map,
  {$IFDEF DOOM}
  r_plane,
  st_stuff,
  {$ENDIF}
  {$IFDEF DOOM_OR_STRIFE}
  r_colormaps,
  {$ENDIF}
  doomdata,
  info_h,
  g_game,
  v_video,
  v_data,
  m_stack,
  info,
  sc_states,
  gl_ambient,
  gl_main,
  gl_misc,
  gl_tex,
  gl_sky,
  gl_lights,
  gl_types,
  r_dynlights,
  gl_models,
  gl_voxels,
  vx_base,
  nd_main,
  gl_automap,
  gl_frustum,
  gl_lightmaps,
  gl_clipper,
  gl_shadows,
  gl_slopes,
  p_3dfloors,
  p_maputl,
  p_mobj_h,
  p_local,
  p_setup,
  p_pspr,
  p_tick,
  r_main,
  r_aspect,
  r_bsp,
  r_draw,
  r_data,
  r_sky,
  r_intrpl,
  r_things,
  r_lights,
  r_renderstyle,
  sc_engine,
  w_wad,
  z_zone;

{$IFDEF DEBUG}
var
  rendered_visplanes,
  rendered_segs,
  rendered_vissprites: integer;
{$ENDIF}

{*
 * lookuptable for lightvalues
 * calculated as follow:
 * floatlight=(gammatable(usegamma, lighttable) + (1.0-exp((light^3)*gamma)) / (1.0-exp(1.0*gamma))) / 2;
 * gamma=-0,2;-2,0;-4,0;-6,0;-8,0
 * usegamme=0;1;2;3;4
 * light=0,0 .. 1,0
 *}

const
  gl_lighttable: array[0..GAMMASIZE - 1, 0..255] of float = (
  (
    0.0019607843, 0.0039215686, 0.0058823529, 0.0078431373, 0.0098039216, 0.0117697059, 0.0137304902, 0.0156962745,
    0.0176620588, 0.0196278431, 0.0215986275, 0.0235694118, 0.0255401961, 0.0275159804, 0.0294967647, 0.0314725490,
    0.0334583333, 0.0354441177, 0.0374299020, 0.0394206863, 0.0414164706, 0.0434172549, 0.0454180392, 0.0474238235,
    0.0494346078, 0.0514503922, 0.0534711765, 0.0554969608, 0.0575227451, 0.0595585294, 0.0615993137, 0.0636450980,
    0.0656958823, 0.0677516667, 0.0698124509, 0.0718832353, 0.0739540196, 0.0760348039, 0.0781255883, 0.0802213726,
    0.0823221569, 0.0844329412, 0.0865487255, 0.0886695099, 0.0908052941, 0.0929460784, 0.0950918627, 0.0972476471,
    0.0994134314, 0.1015892157, 0.1037700001, 0.1059607845, 0.1081615688, 0.1103723531, 0.1125931371, 0.1148189213,
    0.1170597059, 0.1193104901, 0.1215712744, 0.1238420587, 0.1261228432, 0.1284136277, 0.1307144118, 0.1330301962,
    0.1353559803, 0.1376917644, 0.1400425494, 0.1424033330, 0.1447741172, 0.1471599022, 0.1495606861, 0.1519714706,
    0.1543922547, 0.1568280396, 0.1592788234, 0.1617396078, 0.1642153921, 0.1667061763, 0.1692119612, 0.1717277449,
    0.1742635296, 0.1768093130, 0.1793700982, 0.1819458832, 0.1845416675, 0.1871474504, 0.1897682351, 0.1924040197,
    0.1950598035, 0.1977305890, 0.2004113732, 0.2031171560, 0.2058329412, 0.2085687255, 0.2113195098, 0.2140902933,
    0.2168760786, 0.2196768618, 0.2224976462, 0.2253384316, 0.2281942150, 0.2310699996, 0.2339607858, 0.2368715676,
    0.2398023542, 0.2427481388, 0.2457189201, 0.2487047068, 0.2517104910, 0.2547362762, 0.2577820587, 0.2608478424,
    0.2639336271, 0.2670394129, 0.2701651961, 0.2733109803, 0.2764817650, 0.2796675477, 0.2828783346, 0.2861091188,
    0.2893599003, 0.2926356861, 0.2959314691, 0.2992522563, 0.3025930409, 0.3059538229, 0.3093396089, 0.3127503917,
    0.3142203913, 0.3176711799, 0.3211519572, 0.3246527467, 0.3281785329, 0.3317243165, 0.3353000961, 0.3388958806,
    0.3425166691, 0.3461624544, 0.3498382357, 0.3535340218, 0.3572548046, 0.3610005916, 0.3647713753, 0.3685671556,
    0.3723929395, 0.3762437275, 0.3801195122, 0.3840202935, 0.3879510784, 0.3919068600, 0.3958876457, 0.3998984349,
    0.4039342134, 0.4080000028, 0.4120907814, 0.4162115710, 0.4203623567, 0.4245381390, 0.4287439248, 0.4329747073,
    0.4372354933, 0.4415262679, 0.4458470534, 0.4501978498, 0.4545786274, 0.4589844092, 0.4634251938, 0.4678959744,
    0.4723917591, 0.4769225467, 0.4814833303, 0.4860741249, 0.4906949006, 0.4953456872, 0.5000314767, 0.5047472622,
    0.5094930437, 0.5142738281, 0.5190846085, 0.5239253849, 0.5288011791, 0.5337069544, 0.5386477474, 0.5436235284,
    0.5486293203, 0.5536701001, 0.5587458828, 0.5638516615, 0.5689924579, 0.5741682423, 0.5793790145, 0.5846197977,
    0.5899005906, 0.5952113794, 0.6005621630, 0.6059429426, 0.6113637319, 0.6168145172, 0.6223052972, 0.6278310800,
    0.6333918656, 0.6389876541, 0.6446234373, 0.6502942084, 0.6560000122, 0.6617457808, 0.6675265672, 0.6733423415,
    0.6791981404, 0.6850939339, 0.6910247154, 0.6969954916, 0.7030012855, 0.7090470444, 0.7151328575, 0.7212536288,
    0.7274144246, 0.7336151853, 0.7398559706, 0.7461367505, 0.7524575550, 0.7588133474, 0.7652141115, 0.7716549001,
    0.7781306767, 0.7846514846, 0.7912122573, 0.7978180316, 0.8044588236, 0.8111446171, 0.8178704054, 0.8246361883,
    0.8314419660, 0.8382927452, 0.8451885258, 0.8521243012, 0.8591001012, 0.8661208728, 0.8731866757, 0.8802924436,
    0.8874432427, 0.8946340068, 0.9018698021, 0.9091505990, 0.9164763676, 0.9238471675, 0.9312579323, 0.9387137284,
    0.9462195031, 0.9537653022, 0.9613560730, 0.9689968522, 0.9766776558, 0.9844034312, 0.9921792148, 1.0000000000
  ),
  (
    0.0039215686, 0.0078431373, 0.0098039216, 0.0137254902, 0.0156912745, 0.0196178431, 0.0215836275, 0.0235544118,
    0.0274859804, 0.0294617647, 0.0314425490, 0.0353891177, 0.0373749020, 0.0393706863, 0.0413664706, 0.0453330392,
    0.0473438235, 0.0493646079, 0.0513853922, 0.0534211765, 0.0574227451, 0.0594685294, 0.0615243137, 0.0635950981,
    0.0656708824, 0.0677566667, 0.0718132353, 0.0739190196, 0.0760398039, 0.0781705883, 0.0803113725, 0.0824671568,
    0.0846329411, 0.0887745097, 0.0909702942, 0.0931810783, 0.0954018628, 0.0976376471, 0.0998934313, 0.1021642157,
    0.1044450001, 0.1067457842, 0.1110273528, 0.1133631374, 0.1157139214, 0.1180847060, 0.1204754904, 0.1228812746,
    0.1253070589, 0.1277578433, 0.1302236270, 0.1327094118, 0.1352151959, 0.1377409800, 0.1402917647, 0.1428625494,
    0.1474141177, 0.1500299021, 0.1526656867, 0.1553264708, 0.1580072550, 0.1607130396, 0.1634438237, 0.1661996073,
    0.1689803914, 0.1717811765, 0.1746119614, 0.1774627455, 0.1803435294, 0.1832493137, 0.1861800984, 0.1891358816,
    0.1921216665, 0.1951324519, 0.1981732351, 0.2012390187, 0.2043298047, 0.2074505885, 0.2106013740, 0.2137771580,
    0.2169829418, 0.2202187253, 0.2234795093, 0.2267702930, 0.2300910802, 0.2334418635, 0.2368226465, 0.2402334330,
    0.2436692162, 0.2471399985, 0.2506357850, 0.2541615675, 0.2577223528, 0.2613081385, 0.2649289234, 0.2685747049,
    0.2722554893, 0.2759662734, 0.2797020580, 0.2834728417, 0.2872736288, 0.2911044120, 0.2949651987, 0.2988559814,
    0.3027767676, 0.3067325492, 0.3107133349, 0.3147241166, 0.3187699012, 0.3228406900, 0.3269414673, 0.3310722556,
    0.3352380392, 0.3394288270, 0.3436496108, 0.3478953913, 0.3521761747, 0.3545211779, 0.3588569614, 0.3632227485,
    0.3676185315, 0.3720393113, 0.3764850952, 0.3809608826, 0.3854666660, 0.3899974536, 0.3945532379, 0.3991390182,
    0.4037498101, 0.4083855913, 0.4130463766, 0.4177321512, 0.4224429448, 0.4271787276, 0.4319395147, 0.4367202990,
    0.4415260726, 0.4443960810, 0.4492468561, 0.4541226503, 0.4590184269, 0.4639342158, 0.4688700020, 0.4738307775,
    0.4788065734, 0.4838073585, 0.4888231342, 0.4938539153, 0.4989047087, 0.5039754846, 0.5090612808, 0.5141670595,
    0.5192828369, 0.5224578422, 0.5276036305, 0.5327644094, 0.5379401937, 0.5431309836, 0.5483317721, 0.5535425443,
    0.5587683370, 0.5639991215, 0.5692448966, 0.5745006853, 0.5797614659, 0.5850322600, 0.5883472617, 0.5936330465,
    0.5989238231, 0.6042246133, 0.6095254034, 0.6148311706, 0.6201419744, 0.6254527484, 0.6307685291, 0.6360893167,
    0.6414050974, 0.6467258850, 0.6500808814, 0.6554016689, 0.6607174497, 0.6660332305, 0.6713440342, 0.6766548082,
    0.6819555984, 0.6872563588, 0.6925521422, 0.6978429486, 0.7031237116, 0.7064387133, 0.7117045156, 0.7169652813,
    0.7222160930, 0.7274568613, 0.7326876458, 0.7379034398, 0.7431142269, 0.7483099937, 0.7534957768, 0.7567007781,
    0.7618615570, 0.7670023385, 0.7721281295, 0.7772439068, 0.7823397167, 0.7874204763, 0.7924812684, 0.7975270699,
    0.8025528443, 0.8056028437, 0.8105936302, 0.8155644192, 0.8205152110, 0.8254459755, 0.8303567725, 0.8352425356,
    0.8401083310, 0.8449541292, 0.8478141090, 0.8526149057, 0.8573906981, 0.8621464635, 0.8668722476, 0.8715780345,
    0.8762588171, 0.8809096187, 0.8855403931, 0.8881853790, 0.8927611681, 0.8973119530, 0.9018377338, 0.9063385402,
    0.9108093060, 0.9152550973, 0.9196708777, 0.9221008697, 0.9264616648, 0.9307974558, 0.9351032359, 0.9393790050,
    0.9436297998, 0.9478505836, 0.9520463633, 0.9562071550, 0.9583821583, 0.9624879350, 0.9665687373, 0.9706145219,
    0.9746353023, 0.9786260718, 0.9825868602, 0.9865226444, 0.9884626564, 0.9923384187, 0.9961842297, 1.0000000000
  ),
  (
    0.0078431373, 0.0137254902, 0.0176470588, 0.0215736275, 0.0255001961, 0.0294267647, 0.0333583333, 0.0372949020,
    0.0412414706, 0.0432272549, 0.0471838235, 0.0511453922, 0.0531511765, 0.0571327451, 0.0591585294, 0.0631600980,
    0.0652108824, 0.0692324510, 0.0713032353, 0.0753498039, 0.0774505882, 0.0795663726, 0.0836579412, 0.0858087255,
    0.0899302941, 0.0921110785, 0.0943118629, 0.0965326471, 0.1007292157, 0.1029899999, 0.1052657843, 0.1095273530,
    0.1118531371, 0.1141989217, 0.1165697059, 0.1209262744, 0.1233470589, 0.1257928430, 0.1282686272, 0.1307644117,
    0.1352559805, 0.1378117644, 0.1403925488, 0.1430083332, 0.1456541174, 0.1483299022, 0.1529964703, 0.1557372549,
    0.1585080393, 0.1613088235, 0.1641496080, 0.1670203914, 0.1699261757, 0.1728719602, 0.1758477446, 0.1808193142,
    0.1838700980, 0.1869608821, 0.1900866671, 0.1932474513, 0.1964482357, 0.1996890204, 0.2029648043, 0.2062855878,
    0.2096413722, 0.2130421582, 0.2164779414, 0.2199587261, 0.2234795093, 0.2270402946, 0.2306410784, 0.2342868636,
    0.2399334317, 0.2436642169, 0.2474350005, 0.2512457826, 0.2551015700, 0.2589973520, 0.2629381356, 0.2669189213,
    0.2709447048, 0.2750104905, 0.2791212777, 0.2832720558, 0.2874628436, 0.2917036248, 0.2959794088, 0.2983394100,
    0.3027001977, 0.3071059795, 0.3115467640, 0.3160325501, 0.3205583309, 0.3251241213, 0.3297348983, 0.3343806856,
    0.3390664677, 0.3437872525, 0.3485530388, 0.3533538206, 0.3581896051, 0.3630653918, 0.3679761739, 0.3729269582,
    0.3779077459, 0.3829235291, 0.3879743076, 0.3910993195, 0.3962150969, 0.4013658771, 0.4065466682, 0.4117574554,
    0.4169982386, 0.4222690178, 0.4275648012, 0.4328905956, 0.4382413792, 0.4436221588, 0.4490229358, 0.4524879475,
    0.4579337260, 0.4634045086, 0.4688952886, 0.4744060809, 0.4799368556, 0.4854826506, 0.4910484282, 0.4966292112,
    0.5022249997, 0.5058700025, 0.5114957870, 0.5171265634, 0.5227723602, 0.5284231340, 0.5340839213, 0.5397547073,
    0.5454254933, 0.5491454937, 0.5548262784, 0.5605120550, 0.5662028384, 0.5718886150, 0.5775744214, 0.5832601980,
    0.5869801835, 0.5926609831, 0.5983317691, 0.6039975483, 0.6096583207, 0.6153041324, 0.6209449075, 0.6246149145,
    0.6302306854, 0.6358364726, 0.6414272692, 0.6470030454, 0.6525638310, 0.6581046193, 0.6616645961, 0.6671703965,
    0.6726561697, 0.6781169686, 0.6835577403, 0.6889785146, 0.6924085235, 0.6977793193, 0.7031201042, 0.7084358850,
    0.7137266616, 0.7189874571, 0.7222574573, 0.7274632377, 0.7326340301, 0.7377748117, 0.7428805754, 0.7479563581,
    0.7510413753, 0.7560471523, 0.7610229482, 0.7659637263, 0.7708645097, 0.7737744979, 0.7786053054, 0.7833960885,
    0.7881518538, 0.7928726610, 0.7955926594, 0.8002334175, 0.8048342108, 0.8093999863, 0.8139257969, 0.8184115829,
    0.8208915832, 0.8252973500, 0.8296631520, 0.8339839226, 0.8382697053, 0.8405547089, 0.8447554954, 0.8489162872,
    0.8530370544, 0.8551570724, 0.8591978502, 0.8631986333, 0.8671594217, 0.8710801855, 0.8729951933, 0.8768359677,
    0.8806367772, 0.8843975622, 0.8881183227, 0.8898383339, 0.8934841117, 0.8970848881, 0.9006556832, 0.9022206926,
    0.9057164753, 0.9091672565, 0.9125880268, 0.9159688322, 0.9173538355, 0.9206646054, 0.9239403874, 0.9271811815,
    0.9284261735, 0.9315969619, 0.9347377394, 0.9378435289, 0.9409193075, 0.9420043207, 0.9450151004, 0.9479958692,
    0.9509466569, 0.9519116561, 0.9548024516, 0.9576682431, 0.9605040236, 0.9613490088, 0.9641298040, 0.9668855951,
    0.9696163820, 0.9703613805, 0.9730421591, 0.9756979336, 0.9783287337, 0.9789787224, 0.9815645210, 0.9841302924,
    0.9866710895, 0.9891968663, 0.9897368546, 0.9922176596, 0.9946834443, 0.9971242248, 0.9975892304, 1.0000000000
  ),
  (
    0.0156862745, 0.0235294118, 0.0313725490, 0.0372599020, 0.0431472549, 0.0470838235, 0.0529811765, 0.0569227451,
    0.0608793137, 0.0667966667, 0.0707682353, 0.0747498039, 0.0787463726, 0.0807921569, 0.0848087255, 0.0888452941,
    0.0928968627, 0.0969684314, 0.0990942157, 0.1032057844, 0.1053715686, 0.1095181372, 0.1136947059, 0.1159254902,
    0.1201470588, 0.1224328432, 0.1267094118, 0.1290451961, 0.1314159805, 0.1357775488, 0.1382083334, 0.1426299021,
    0.1451256861, 0.1476514708, 0.1521780391, 0.1547738234, 0.1574096079, 0.1600853919, 0.1647569611, 0.1675077454,
    0.1702985290, 0.1731293139, 0.1779658827, 0.1808816666, 0.1838424511, 0.1868482352, 0.1898940197, 0.1949505874,
    0.1980963716, 0.2012821562, 0.2045179416, 0.2078037260, 0.2111395094, 0.2145252936, 0.2199168636, 0.2234026477,
    0.2269384307, 0.2305292158, 0.2341700017, 0.2378607848, 0.2416065680, 0.2454073515, 0.2492581357, 0.2531639202,
    0.2571247048, 0.2611404897, 0.2652112747, 0.2693320605, 0.2735128422, 0.2777486278, 0.2820344104, 0.2863801964,
    0.2907759832, 0.2952267664, 0.2997375492, 0.3042983328, 0.3089141166, 0.3135799013, 0.3183006861, 0.3230764712,
    0.3279072564, 0.3327880424, 0.3377188219, 0.3427046090, 0.3477353901, 0.3528211789, 0.3579519617, 0.3631377448,
    0.3683635300, 0.3736443153, 0.3789650954, 0.3843358838, 0.3877858826, 0.3932466666, 0.3987424534, 0.4042832417,
    0.4098690166, 0.4154898092, 0.4211455897, 0.4268463718, 0.4325771499, 0.4363821614, 0.4421829452, 0.4480137249,
    0.4538795076, 0.4597702943, 0.4656910771, 0.4716368641, 0.4776126471, 0.4816476431, 0.4876634357, 0.4937042176,
    0.4997600049, 0.5058357896, 0.5119265648, 0.5160665693, 0.5221873555, 0.5283131336, 0.5344489252, 0.5405947007,
    0.5467454829, 0.5509404876, 0.5571012685, 0.5632570575, 0.5694178384, 0.5755736423, 0.5817244246, 0.5859094157,
    0.5920501843, 0.5981809692, 0.6043017703, 0.6104075510, 0.6145375568, 0.6206183334, 0.6266791126, 0.6327198945,
    0.6387406790, 0.6427806818, 0.6487564648, 0.6547072436, 0.6606330481, 0.6645730344, 0.6704388170, 0.6762795954,
    0.6820853860, 0.6878611656, 0.6916361662, 0.6973419699, 0.7030077491, 0.7086385404, 0.7122685229, 0.7178193248,
    0.7233301021, 0.7287958780, 0.7322608748, 0.7376416544, 0.7429774623, 0.7482682389, 0.7515482229, 0.7567440195,
    0.7618948146, 0.7669956014, 0.7700855956, 0.7750863658, 0.7800421643, 0.7849429478, 0.7878329386, 0.7926337352,
    0.7973795167, 0.8001145057, 0.8047603004, 0.8093560869, 0.8138918514, 0.8164218600, 0.8208576378, 0.8252384303,
    0.8276084302, 0.8318892061, 0.8361149967, 0.8402907790, 0.8424507917, 0.8465265574, 0.8505473677, 0.8525523487,
    0.8564731424, 0.8603439277, 0.8641647048, 0.8659747190, 0.8696954795, 0.8733712683, 0.8750362644, 0.8786120664,
    0.8821478438, 0.8836728287, 0.8871136261, 0.8905094220, 0.8919044091, 0.8952152089, 0.8984809773, 0.9017067510,
    0.9029317754, 0.9060775597, 0.9091783424, 0.9102883299, 0.9133141300, 0.9163049124, 0.9172998927, 0.9202206993,
    0.9231064583, 0.9239964748, 0.9268122580, 0.9295980302, 0.9303930369, 0.9331188170, 0.9358096092, 0.9365146130,
    0.9391503901, 0.9417611631, 0.9423811706, 0.9449369582, 0.9474727486, 0.9480177436, 0.9505035257, 0.9529643035,
    0.9534393228, 0.9558601059, 0.9582558849, 0.9586708823, 0.9610316734, 0.9614116530, 0.9637324493, 0.9660382253,
    0.9663632495, 0.9686390145, 0.9708948120, 0.9711748048, 0.9734055982, 0.9756163644, 0.9758613693, 0.9780471611,
    0.9802279462, 0.9804329266, 0.9825837305, 0.9827687135, 0.9849044970, 0.9870302968, 0.9871852987, 0.9892910713,
    0.9913918669, 0.9915218647, 0.9936026331, 0.9956784246, 0.9957884250, 0.9978542029, 0.9979492127, 1.0000000000
  ),
  (
    0.0313725490, 0.0450980392, 0.0549019608, 0.0627500980, 0.0706032353, 0.0765005882, 0.0824029412, 0.0883202941,
    0.0942426471, 0.0982142157, 0.1041615686, 0.1081631372, 0.1121797059, 0.1181770588, 0.1222286275, 0.1263051961,
    0.1303967648, 0.1345183333, 0.1366991176, 0.1408656863, 0.1450622549, 0.1492888235, 0.1515846078, 0.1558661764,
    0.1601877450, 0.1625785294, 0.1669700978, 0.1694308822, 0.1738974511, 0.1764382351, 0.1809848041, 0.1836105881,
    0.1882371572, 0.1909479412, 0.1937087251, 0.1984752939, 0.2013310782, 0.2042318632, 0.2091484317, 0.2121492156,
    0.2152099999, 0.2183207841, 0.2234473537, 0.2266681374, 0.2299439213, 0.2332797065, 0.2366704901, 0.2401212750,
    0.2455978431, 0.2491686271, 0.2528044119, 0.2565001961, 0.2602559799, 0.2640767644, 0.2679625478, 0.2719133337,
    0.2759291186, 0.2800049029, 0.2841506855, 0.2883614706, 0.2926372546, 0.2969780375, 0.3013888223, 0.3078253903,
    0.3104003929, 0.3150111774, 0.3196819577, 0.3244227473, 0.3292235290, 0.3340943164, 0.3390300952, 0.3440308803,
    0.3490916650, 0.3542174485, 0.3594082383, 0.3646640195, 0.3699748009, 0.3753505886, 0.3807863759, 0.3862771559,
    0.3898671586, 0.3954779376, 0.4011387249, 0.4068595118, 0.4126302921, 0.4184560800, 0.4243318613, 0.4302576509,
    0.4342676427, 0.4402884272, 0.4463492214, 0.4524599940, 0.4586057843, 0.4647965612, 0.4690615716, 0.4753273461,
    0.4816231315, 0.4879539198, 0.4943147040, 0.4987447001, 0.5051604845, 0.5116012732, 0.5180670659, 0.5245528412,
    0.5290928375, 0.5356136305, 0.5421444072, 0.5486901895, 0.5532852009, 0.5598459737, 0.5664167601, 0.5729875464,
    0.5775975484, 0.5841683348, 0.5907341144, 0.5972948871, 0.6018898986, 0.6084306740, 0.6149614657, 0.6214772667,
    0.6260122563, 0.6324930397, 0.6389538257, 0.6434288232, 0.6498396009, 0.6562203974, 0.6625761898, 0.6669361802,
    0.6732269737, 0.6794827495, 0.6837427531, 0.6899235164, 0.6960643149, 0.7001993275, 0.7062550999, 0.7122608939,
    0.7182266633, 0.7221816699, 0.7280474525, 0.7338632268, 0.7376632315, 0.7433740123, 0.7490298077, 0.7526698038,
    0.7582105921, 0.7636963653, 0.7671663689, 0.7725321579, 0.7778429319, 0.7811329295, 0.7863187125, 0.7894887259,
    0.7945495180, 0.7995552950, 0.8025302820, 0.8074110681, 0.8122218485, 0.8150118525, 0.8197026488, 0.8243234394,
    0.8269234238, 0.8314242302, 0.8338992237, 0.8382700026, 0.8425807894, 0.8448657931, 0.8490515592, 0.8531773630,
    0.8552823607, 0.8592881505, 0.8612731342, 0.8651589101, 0.8689897008, 0.8707997150, 0.8745104917, 0.8762104758,
    0.8798162887, 0.8833620499, 0.8848970483, 0.8883428526, 0.8897778344, 0.8931186152, 0.8964144244, 0.8976944043,
    0.9008952037, 0.9020852104, 0.9051909701, 0.9082517582, 0.9093117672, 0.9122875590, 0.9132625420, 0.9161583443,
    0.9170533379, 0.9198741278, 0.9226599000, 0.9234448932, 0.9261556828, 0.9268756771, 0.9295214678, 0.9301814700,
    0.9327672686, 0.9353230265, 0.9358880487, 0.9383888213, 0.9389038352, 0.9413545995, 0.9418196052, 0.9442203910,
    0.9446403951, 0.9469961794, 0.9493319663, 0.9496919486, 0.9519877408, 0.9523077581, 0.9545685326, 0.9548535322,
    0.9570843256, 0.9573393143, 0.9595400967, 0.9597651042, 0.9619358757, 0.9621358791, 0.9642816762, 0.9664224368,
    0.9665874522, 0.9687032384, 0.9688482267, 0.9709440155, 0.9710740133, 0.9731548115, 0.9732648119, 0.9753305898,
    0.9754305766, 0.9774813639, 0.9775663601, 0.9796071637, 0.9796771693, 0.9817079295, 0.9817729284, 0.9837937346,
    0.9838487199, 0.9858595125, 0.9859045140, 0.9879102998, 0.9879502945, 0.9899510735, 0.9899860913, 0.9919768567,
    0.9920068677, 0.9939926561, 0.9940176603, 0.9960034189, 0.9979842304, 0.9980042278, 0.9999850094, 1.0000000000
  ));

function gld_CalcLightLevel(lightlevel: integer): float;
begin
  result := gl_lighttable[usegamma][gl_i_max(gl_i_min((lightlevel), 255), 0)];
end;


procedure gld_StaticLightAlpha(light: float; alpha: float);
begin
  if players[displayplayer].fixedcolormap <> 0 then
    glColor4f(1.0, 1.0, 1.0, alpha)
  else
    glColor4f(light, light, light, alpha);
end;

procedure gld_StaticLight(light: float);
begin
  if players[displayplayer].fixedcolormap <> 0 then
    glColor4f(1.0, 1.0, 1.0, 1.0)
  else
    glColor4f(light, light, light, 1.0);
end;

procedure gld_InitExtensions(ext_list: TDStringList);
begin
  gl_texture_filter_anisotropic := ext_list.IndexOf('GL_EXT_TEXTURE_FILTER_ANISOTROPIC') > -1;
  if gl_texture_filter_anisotropic then
    printf('enabled anisotropic texture filtering'#13#10);
  if gl_use_paletted_texture <> 0 then
  begin
    gl_paletted_texture := ext_list.IndexOf('GL_EXT_PALETTED_TEXTURE') > -1;
    gld_ColorTableEXT := lp3DFXFUNC(wglGetProcAddress('glColorTableEXT'));
    if not Assigned(gld_ColorTableEXT) then
      gl_paletted_texture := false
    else
      printf('using GL_EXT_paletted_texture'#13#10);
  end;
  if gl_use_shared_texture_palette <> 0 then
  begin
    gl_shared_texture_palette := ext_list.IndexOf('GL_EXT_SHARED_TEXTURE_PALETTE') > -1;
    gld_ColorTableEXT := lp3DFXFUNC(wglGetProcAddress('glColorTableEXT'));
    if not Assigned(gld_ColorTableEXT) then
      gl_shared_texture_palette := false
    else
      printf('using GL_EXT_shared_texture_palette'#13#10);
  end;

  canuselightmaps := ext_list.IndexOf('GL_ARB_MULTITEXTURE') > -1;
  if not canuselightmaps then
  begin
    I_Warning('gld_InitExtensions(): GL_ARB_MULTITEXTURE extension not supported, lightmap will be disabled'#13#10);
    gl_uselightmaps := false;
  end
  else
  begin
    canuselightmaps := ext_list.IndexOf('GL_EXT_TEXTURE3D') > -1;
    if not canuselightmaps then
    begin
      I_Warning('gld_InitExtensions(): GL_EXT_TEXTURE3D extension not supported, lightmap will be disabled'#13#10);
      gl_uselightmaps := false;
    end;
  end;
  gld_InitAutomap;
end;

{-------------------------------------------------------------------}
{ V-Sync
{ Ok for all system windows 32                                      }
{-------------------------------------------------------------------}
type
  TVSyncMode = (vsmSync, vsmNoSync);

var
  gld_VSync_Warning: boolean = false;

function gld_VSync(vsync: TVSyncMode): boolean;
var
  i: Integer;
begin
  if WGL_EXT_swap_control then
  begin
    result := True;
    i := wglGetSwapIntervalEXT;
    case VSync of
      vsmSync    : if i <> 1 then wglSwapIntervalEXT(1);
      vsmNoSync  : if i <> 0 then wglSwapIntervalEXT(0);
    else
      Assert(False);
    end;
  end
  else
  begin
    result := False;
    if not gld_VSync_Warning then
    begin
      I_Warning('gld_VSync(): wglSwapIntervalEXT() not assigned'#13#10);
      gld_VSync_Warning := true;
    end;
  end;
end;

var
  gld_InitLightTable_initialized: Boolean = false;

procedure gld_InitLightTable;
var
  sc: TScriptEngine;
  lump: integer;
  i, j: integer;
begin
  if gld_InitLightTable_initialized then
    Exit;

  lump := W_CheckNumForName('GLGAMMA');
  if lump > 0 then
  begin

    sc := TScriptEngine.Create(W_TextLumpNum(lump));
    for i := 0 to GAMMASIZE - 1 do
      for j := 0 to 255 do
      begin
        sc.MustGetFloat;
        gl_lighttable[i, j] := sc._Float;
      end;
    sc.Free;
  end;

  for i := 0 to GAMMASIZE - 1 do
    for j := 0 to 255 do
      gl_lighttable[i, j] := Sqrt(gl_lighttable[i, j]);

  gld_InitLightTable_initialized := true;
end;

var
  last_screensync: boolean;

procedure gld_Init(width, height: integer);
var
  params: array[0..3] of TGLfloat;
  BlackFogColor: array[0..3] of TGLfloat;
  ext_lst: TDStringList;
  i, tf_id: integer;
  extensions,
  extensions_l: string;
begin
  params[0] := 0.0;
  params[1] := 0.0;
  params[2] := 1.0;
  params[3] := 0.0;
  BlackFogColor[0] := 0.0;
  BlackFogColor[1] := 0.0;
  BlackFogColor[2] := 0.0;
  BlackFogColor[3] := 0.0;

  printf('GL_VENDOR: %s'#13#10 , [glGetString(GL_VENDOR)]);
  printf('GL_RENDERER: %s'#13#10, [glGetString(GL_RENDERER)]);
  printf('GL_VERSION: %s'#13#10, [glGetString(GL_VERSION)]);

  if devparm then
    printf('GL_EXTENSIONS:'#13#10);

  extensions := StringVal(glGetString(GL_EXTENSIONS));
  extensions_l := '';
  for i := 1 to Length(extensions) do
  begin
    if extensions[i] = ' ' then
      extensions_l := extensions_l + #13#10
    else
      extensions_l := extensions_l + toupper(extensions[i]);
  end;

  ext_lst := TDStringList.Create;
  try
    ext_lst.Text := extensions_l;
    if devparm then
      for i := 0 to ext_lst.count - 1 do
        printf('  %s'#13#10, [ext_lst.strings[i]]);
    gld_InitExtensions(ext_lst);
  finally
    ext_lst.Free;
  end;

  gld_InitPalettedTextures;

  glViewport(0, 0, SCREENWIDTH, SCREENHEIGHT);

  {$IFDEF DEBUG}
  glClearColor(0.0, 0.5, 0.5, 1.0);
  glClearDepth(1.0);
  {$ELSE}
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClearDepth(1.0);
  {$ENDIF}

  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @gld_max_texturesize);
  printf('GL_MAX_TEXTURE_SIZE=%d'#13#10, [gld_max_texturesize]);
  glGetIntegerv(GL_MAX_3D_TEXTURE_SIZE, @gld_max_texturesize3d);
  printf('GL_MAX_3D_TEXTURE_SIZE=%d'#13#10, [gld_max_texturesize3d]);

  if canuselightmaps then
    canuselightmaps := (gld_max_texturesize3d >= LIGHTMAPSIZEX) and
                       (gld_max_texturesize3d >= LIGHTMAPSIZEY) and
                       (gld_max_texturesize3d >= LIGHTMAPSIZEZ);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glShadeModel(GL_FLAT);
  glEnable(GL_TEXTURE_2D);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GEQUAL, 0.5);
  glDisable(GL_CULL_FACE);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  glTexGenfv(GL_Q, GL_EYE_PLANE, @params);
  glTexGenf(GL_S,GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  glTexGenf(GL_T,GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  glTexGenf(GL_Q,GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  glFogi(GL_FOG_MODE, GL_EXP);
  glFogfv(GL_FOG_COLOR, @BlackFogColor);
  glFogf(GL_FOG_DENSITY, fog_density / 1000.0);
  glHint(GL_FOG_HINT, GL_NICEST);
  glFogf(GL_FOG_START, 0.0);
  glFogf(GL_FOG_END, 1.0);

  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);

// Texture filtering and mipmaping
  gld_SetCurrTexFiltering(gld_GetCurrTexFiltering);

// Texture format
  gl_tex_format := DEF_TEX_FORMAT;
  gl_tex_format_string := strupper(gl_tex_format_string);
  tf_id := -1;
  for i := 0 to NUM_GL_TEX_FORMATS - 1 do
    if gl_tex_format_string = gl_tex_formats[i].desc then
    begin
      gl_tex_format := gl_tex_formats[i].tex_format;
      tf_id := i;
      break;
    end;

  if tf_id < 0 then
    printf('Using default texture format.'#13#10)
  else
    printf('Using texture format %s.'#13#10, [gl_tex_format_string]);

  if gl_screensync then
    gld_VSync(vsmSync)
  else
    gld_VSync(vsmNoSync);
  last_screensync := gl_screensync;

  gld_InitLightTable;
  gld_CalculateSky(100000.0);
  R_InitDynamicLights;
  gld_InitDynamicShadows;
  gld_InitModels;
  gld_InitVoxels;
  gld_InitLightmap;
  gld_InitAmbient;
end;


procedure gld_DrawNumPatch(x, y: integer; lump: integer; cm: integer; flags: integer;
  const zoomx: float = 1.0; const zoomy: float = 1.0);

  function SCALE_X(const xx: integer): float;
  begin
    if flags and VPT_STRETCH <> 0 then
      result := xx * SCREENWIDTH / 320.0
    else
      result := xx;
  end;

  function SCALE_Y(const yy: integer): float;
  begin
    if flags and VPT_STRETCH <> 0 then
      result := yy * SCREENHEIGHT / 200.0
    else
      result := yy;
  end;

var
  gltexture: PGLTexture;
  fU1, fU2, fV1, fV2: float;
  width, height: float;
  xpos, ypos: float;
  dx, dy: float;
begin
  if flags and VPT_TRANS <> 0 then
  begin
    gltexture := gld_RegisterPatch(lump, cm, flags and VPT_NOUNLOAD = 0);
    gld_BindPatch(gltexture, cm);
  end
  else
  begin
    gltexture := gld_RegisterPatch(lump, Ord(CR_DEFAULT), flags and VPT_NOUNLOAD = 0);
    gld_BindPatch(gltexture, Ord(CR_DEFAULT));
  end;
  if gltexture = nil then
    exit;
  fV1 := 0.0;
  fV2 := gltexture.height / gltexture.tex_height;
  if flags and VPT_FLIP <> 0 then
  begin
    fU1 := gltexture.width / gltexture.tex_width;
    fU2 := 0.0;
  end
  else
  begin
    fU1 := 0.0;
    fU2 := gltexture.width / gltexture.tex_width;
  end;
  xpos := SCALE_X(x - gltexture.leftoffset);
  ypos := SCALE_Y(y - gltexture.topoffset);
  width := SCALE_X(gltexture.realtexwidth);
  height := SCALE_Y(gltexture.realtexheight);
  if (zoomx <> 1.0) or (zoomy <> 1.0) then
  begin
    dx := width * zoomx - width;
    dy := height * zoomy - height;
    xpos := xpos - dx / 2;
    ypos := ypos - dy / 2;
    width := width + dx;
    height := height + dy;
  end;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(fU1, fV1);
    glVertex2f(xpos, ypos);
    glTexCoord2f(fU1, fV2);
    glVertex2f(xpos, ypos + height);
    glTexCoord2f(fU2, fV1);
    glVertex2f(xpos + width, ypos);
    glTexCoord2f(fU2, fV2);
    glVertex2f(xpos + width, ypos + height);
  glEnd;
end;

procedure gld_DrawBackground(const name: string);
var
  gltexture: PGLTexture;
  fU1, fU2, fV1, fV2: float;
  width, height: float;
{$IFDEF DOOM}
  sbar: Integer;
{$ENDIF}
begin
  gltexture := gld_RegisterFlat(W_GetNumForName(name), false, -1);
  gld_BindFlat(gltexture);
  if gltexture = nil then
    exit;
  fU1 := 0;
  fV1 := 0;
  fU2 := 320 / gltexture.realtexwidth;
  {$IFDEF DOOM}
  sbar := ST_HEIGHT;
  {$ENDIF}
  fV2 := (200{$IFDEF DOOM} - sbar{$ENDIF}) / gltexture.realtexheight;
  width := SCREENWIDTH;
  height := SCREENHEIGHT{$IFDEF DOOM} - sbar * SCREENHEIGHT / 200{$ENDIF};

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(fU1, fV1);
    glVertex2f(0.0, 0.0);
    glTexCoord2f(fU1, fV2);
    glVertex2f(0.0, height);
    glTexCoord2f(fU2, fV1);
    glVertex2f(width, 0);
    glTexCoord2f(fU2, fV2);
    glVertex2f(width, height);
  glEnd;
end;

var
  scissoron: boolean = false;

procedure gld_DrawBackgroundFrame;
var
  x1, x2, y1, y2: integer;
begin
  if (viewwindowx <= 0) and (viewwindowy <= 0) then
    Exit;

  x1 := viewwindowx - 1;
  x2 := x1 + viewwidth + 2;
  y1 := viewwindowy - 1;
  y2 := y1 + viewheight + 2;

  glDisable(GL_TEXTURE_2D);

  glColor4f(0.5, 0.5, 0.5, 1.0);
  glBegin(GL_LINE_STRIP);
    glVertex2i(x1, y1);
    glVertex2i(x2, y1);
    glVertex2i(x2, y2);
    glVertex2i(x1, y2);
    glVertex2i(x1, y1);
  glEnd;

  glEnable(GL_TEXTURE_2D);
end;

procedure gld_DrawLine(x0, y0, x1, y1: integer; PalColor: byte);
var
  playpal: PByteArray;
  idx: integer;
begin
  glBindTexture(GL_TEXTURE_2D, 0);
  last_gltexture := nil;
  last_cm := -1;

  playpal := V_ReadPalette(PU_STATIC);
  idx := 3 * PalColor;
  glColor3f(playpal[idx] / 255.0,
            playpal[idx + 1] / 255.0,
            playpal[idx + 2] / 255.0);
  Z_ChangeTag(playpal, PU_CACHE);

  glBegin(GL_LINES);
    glVertex2i(x0, y0);
    glVertex2i(x1, y1);
  glEnd;
end;

procedure gld_DrawWeapon(weaponlump: integer; vis: Pvissprite_t; lightlevel: integer);
var
  gltexture: PGLTexture;
  fU1, fU2, fV1, fV2: float;
  x1, y1, x2, y2: integer;
  scale: float;
  light: float;
  restoreblend: boolean;
begin
  gltexture := gld_RegisterPatch(firstspritelump + weaponlump, Ord(CR_DEFAULT));
  if gltexture = nil then
    exit;
  gld_BindPatch(gltexture, Ord(CR_DEFAULT));
  fU1 := 0.0;
  fV1 := 0.0;
  fU2 := gltexture.width / gltexture.tex_width;
  fV2 := gltexture.height / gltexture.tex_height;
  x1 := viewwindowx + vis.x1;
  x2 := viewwindowx + vis.x2;
  scale := vis.scale / FRACUNIT;
  y1 := viewwindowy + centery - round((vis.texturemid / FRACUNIT) * scale);
  y2 := y1 + round(gltexture.realtexheight * scale) + 1;
  light := gld_CalcLightLevel(lightlevel);

// JVAL??  viewplayer.mo.renderstyle = mrs_translucent
  if viewplayer.mo.flags and MF_SHADOW <> 0 then
  begin
    glBlendFunc(GL_DST_COLOR, GL_ONE_MINUS_SRC_ALPHA);
    glAlphaFunc(GL_GEQUAL, 0.1);
    glColor4f(0.2, 0.2, 0.2, 0.33);
    restoreblend := true;
  end
  else
  begin
    if (viewplayer.mo.flags_ex and MF_EX_TRANSPARENT <> 0) or
       (viewplayer.mo.renderstyle = mrs_translucent) then
    begin
      gld_StaticLightAlpha(light, tran_filter_pct / 100.0);
      restoreblend := true;
    end
    else
    begin
      gld_StaticLight(light);
      restoreblend := false;
    end;
  end;
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(fU1, fV1);
    glVertex2f(x1, y1);
    glTexCoord2f(fU1, fV2);
    glVertex2f(x1, y2);
    glTexCoord2f(fU2, fV1);
    glVertex2f(x2, y1);
    glTexCoord2f(fU2, fV2);
    glVertex2f(x2, y2);
  glEnd;
  if restoreblend then
  begin
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glAlphaFunc(GL_GEQUAL, 0.5);
  end;
  glColor3f(1.0, 1.0, 1.0);
end;

procedure gld_FillBlock(x, y, width, height: integer; col: integer);
var
  playpal: PByteArray;
begin
  glBindTexture(GL_TEXTURE_2D, 0);
  last_gltexture := nil;
  last_cm := -1;
  playpal := V_ReadPalette(PU_STATIC);
  glColor3f(playpal[3 * col] / 255.0,
            playpal[3 * col + 1] / 255.0,
            playpal[3 * col + 2] / 255.0);
  Z_ChangeTag(playpal, PU_CACHE);
  glBegin(GL_TRIANGLE_STRIP);
    glVertex2i(x, y);
    glVertex2i(x, y + height);
    glVertex2i(x + width, y);
    glVertex2i(x + width, y + height);
  glEnd;
  glColor3f(1.0, 1.0, 1.0);
end;

var
  last_palette: integer = 0;

procedure gld_SetPalette(palette: integer);
var
  playpal: PByteArray;
  plpal: PByteArray;
  pal: array[0..1023] of byte;
  i: integer;
  col, pcol: integer;
begin
  extra_red := 0.0;
  extra_green := 0.0;
  extra_blue := 0.0;
  extra_alpha := 0.0;
  if palette < 0 then
    palette := last_palette;
  last_palette := palette;
  if gl_shared_texture_palette then
  begin

    playpal := V_ReadPalette(PU_STATIC);
    plpal := @playpal[768];
    if fixedcolormap <> nil then
    begin
      for i := 0 to 255 do
      begin
        col := 3 * fixedcolormap[i];
        pcol := i * 4;
        pal[pcol] := plpal[col];
        pal[pcol + 1] := plpal[col + 1];
        pal[pcol + 2] := plpal[col + 2];
        pal[pcol + 3] := 255;
      end;
    end
    else
    begin
      for i := 0 to 255 do
      begin
        col := 3 * i;
        pcol := col + i;
        pal[pcol] := plpal[col];
        pal[pcol + 1] := plpal[col + 1];
        pal[pcol + 2] := plpal[col + 2];
        pal[pcol + 3] := 255;
      end;
    end;

    Z_ChangeTag(playpal, PU_CACHE);
    pcol := transparent_pal_index * 4;
    pal[pcol] := 0;
    pal[pcol + 1] := 0;
    pal[pcol + 2] := 0;
    pal[pcol + 3] := 0;
    gld_ColorTableEXT(GL_SHARED_TEXTURE_PALETTE_EXT, GL_RGBA, 256, GL_RGBA, GL_UNSIGNED_BYTE, @pal);
  end
  else
  begin
    if palette > 0 then
    begin
      if palette <= 8 then
      begin
        extra_red := palette / 2.0;
        extra_green := 0.0;
        extra_blue := 0.0;
        extra_alpha := palette / 10.0;
      end
      else if palette <= 12 then
      begin
        palette := palette - 8;
        extra_red := palette * 1.0;
        extra_green := palette * 0.8;
        extra_blue := palette * 0.1;
        extra_alpha := palette / 11.0;
      end
      else if palette = 13 then
      begin
        extra_red := 0.4;
        extra_green := 1.0;
        extra_blue := 0.0;
        extra_alpha := 0.2;
      end
      {$IFDEF  HEXEN}
      else if palette < 21 then
      begin
        palette := palette - 13;
        extra_red := 0.0;
        extra_green := palette / 2.0;
        extra_blue := 0.0;
        extra_alpha := palette / 10.0;
      end
      else if palette = 21 then
      begin
        extra_red := 0.0;
        extra_green := 0.0;
        extra_blue := 1.0;
        extra_alpha := 0.8;
      end
      {$ENDIF}
      ;
    end;
    if extra_red > 1.0 then
      extra_red := 1.0;
    if extra_green > 1.0 then
      extra_green := 1.0;
    if extra_blue > 1.0 then
      extra_blue := 1.0;
    if extra_alpha > 1.0 then
      extra_alpha := 1.0;
  end;
end;

procedure gld_Enable2D;
var
  vPort: array[0..3] of GLInt;
begin
  glGetIntegerv(GL_VIEWPORT, @vPort);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;

  glOrtho(0, vPort[2], 0, vPort[3], -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;

  glMatrixMode(GL_TEXTURE);
  glPushMatrix;
  glLoadIdentity;
end;

procedure gld_Disable2D;
begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
  glMatrixMode(GL_TEXTURE);
  glPopMatrix;
end;


procedure gld_Set2DMode;
begin
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(
    0,
    SCREENWIDTH,
    SCREENHEIGHT,
    0,
    -1.0,
    1.0
  );
  glDisable(GL_DEPTH_TEST);
end;

procedure gld_Finish;
begin
  gld_Set2DMode;
  // JVAL:
  //  Hack to avoid Intel HD4000 problem with Win10
  //  https://communities.intel.com/thread/117626
  if not gl_no_glfinish_hack then
    glFinish;
  glFlush;
  SwapBuffers(h_DC);
end;

{*****************
 *               *
 * structs       *
 *               *
 *****************}

var
  gld_max_vertexes: integer = 0;
  gld_num_vertexes: integer = 0;
  gld_vertexes: PGLVertexArray = nil;
  gld_texcoords: PGLTexcoordArray = nil;

procedure gld_AddGlobalVertexes(count: integer);
begin
  if (gld_num_vertexes + count) >= gld_max_vertexes then
  begin
    gld_max_vertexes := gld_max_vertexes + count + 1024;
    gld_vertexes := Z_Realloc(gld_vertexes, gld_max_vertexes * SizeOf(GLVertex), PU_LEVEL, nil);
    gld_texcoords := Z_Realloc(gld_texcoords, gld_max_vertexes * SizeOf(GLTexcoord), PU_LEVEL, nil);
  end;
end;

{* GLLoopDef is the struct for one loop. A loop is a list of vertexes
 * for triangles, which is calculated by the gluTesselator in gld_PrecalculateSector
 * and in gld_PreprocessCarvedFlat
 *}

type
  GLLoopDef = record
    mode: TGLenum;        // GL_TRIANGLES, GL_TRIANGLE_STRIP or GL_TRIANGLE_FAN
    vertexcount: integer; // number of vertexes in this loop
    vertexindex: integer; // index into vertex list
  end;
  PGLLoopDef = ^GLLoopDef;
  GLLoopDefArray = array[0..$FFFF] of GLLoopDef;
  PGLLoopDefArray = ^GLLoopDefArray;

// GLSector is the struct for a sector with a list of loops.

  GLSector = record
    loopcount: integer;     // number of loops for this sector
    loops: PGLLoopDefArray; // the loops itself
    list: GLuint;
  end;
  PGLSector = ^GLSector;
  GLSectorArray = array[0..$FFFF] of GLSector;
  PGLSectorArray = ^GLSectorArray;

  GLSubSector = record
    loop: GLLoopDef; // the loops itself
  end;

  TGLSeg = record
    x1, x2: float;
    z1, z2: float;
    frontsector: Psector_t;
    backsector: Psector_t;
  end;
  PGLSeg = ^TGLSeg;
  GLSegArray = array[0..$FFFF] of TGLSeg;
  PGLSegArray = ^GLSegArray;

var
  gl_segs: PGLSegArray = nil;

const
  GLDWF_TOP = 1;
  GLDWF_M1S = 2;
  GLDWF_MTS = 3;  // Thick Side
  GLDWF_M2S = 4;
  GLDWF_BOT = 5;
  GLDWF_TOPFLUD = 6; //e6y: project the ceiling plane into the gap
  GLDWF_BOTFLUD = 7; //e6y: project the floor plane into the gap
  GLDWF_SKY = 8;
  GLDWF_SKYFLIP = 9;

type
  glwalltype_t = (glwbottom, glwtop, glwmid);
  GLWall = record
    glseg: PGLSeg;
    ytop, ybottom, ymid: float;
    ul, ur, vt, vb: float;
    light: float;
    light2: float;
    whitefog: boolean;  // JVAL: Mars fog sectors
    whitefog2: boolean; // JVAL: Mars fog sectors
    doublelight: boolean;
    alpha: float;
    skyymid: float;
    skyyaw: float;
    gltexture: PGLTexture;
    flag: byte;
    blend: boolean;
  end;
  PGLWall = ^GLWall;
  GLWallArray = array[0..$FFFF] of GLWall;
  PGLWallArray = ^GLWallArray;

  GLFlat = record
    sectornum: integer;
    light: float; // the lightlevel of the flat
    z: float; // the z position of the flat (height)
    angle: float;
    anglex, angley: float;
    hasangle: boolean;
    gltexture: PGLTexture;
    {$IFNDEF HERETIC}
    uoffs, voffs: float; // the texture coordinates
    hasoffset: boolean;
    {$ENDIF}
    ripple: boolean;
    whitefog: boolean; // JVAL: Mars fog sectors
    ceiling: boolean;
  end;
  PGLFlat = ^GLFlat;
  GLFlatArray = array[0..$FFFF] of GLFlat;
  PGLFlatArray = ^GLFlatArray;

const
  GLS_SHADOW = 1;
  GLS_TRANSPARENT = 2;
  GLS_CLIPPED = 4;
  GLS_WHITELIGHT = 8;
  GLS_REDLIGHT = 16;
  GLS_GREENLIGHT = 32;
  GLS_BLUELIGHT = 64;
  GLS_YELLOWLIGHT = 128;
  GLS_LIGHT = GLS_WHITELIGHT or GLS_REDLIGHT or GLS_GREENLIGHT or GLS_BLUELIGHT or GLS_YELLOWLIGHT;
  GLS_ADDITIVE = 256;
  GLS_SUBTRACTIVE = 512;

type
  GLSprite = record
    cm: integer;
    x, y, z: float;
    vt, vb: float;
    ul, ur: float;
    x1, y1: float;
    x2, y2: float;
    light: float;
    scale: fixed_t;
    gltexture: PGLTexture;
    flags: integer;
    alpha: float;
    dlights: T2DNumberList;
    models: TDNumberList;
    voxels: TDNumberList;
    mo: Pmobj_t;
    aproxdist: fixed_t;
  end;
  PGLSprite = ^GLSprite;
  GLSpriteArray = array[0..$FFFF] of GLSprite;
  PGLSpriteArray = ^GLSpriteArray;

  GLDrawItemType = (
    GLDIT_NONE,
    GLDIT_WALL,
    GLDIT_FLAT,
    GLDIT_SPRITE,
    GLDIT_DLIGHT
  );

  GLDrawItem = record
    itemtype: GLDrawItemType;
    itemcount: integer;
    firstitemindex: integer;
    rendermarker: byte;
  end;
  PGLDrawItem = ^GLDrawItem;
  GLDrawItemArray = array[0..$FFFF] of GLDrawItem;
  PGLDrawItemArray = ^GLDrawItemArray;

  GLDrawInfo = record
    walls: PGLWallArray;
    num_walls: integer;
    max_walls: integer;
    flats: PGLFlatArray;
    num_flats: integer;
    max_flats: integer;
    sprites: PGLSpriteArray;
    num_sprites: integer;
    max_sprites: integer;
    drawitems: PGLDrawItemArray;
    num_drawitems: integer;
    max_drawitems: integer;
  end;

type
  gl_strip_coords_t = record
    v: array[0..3] of array [0..2] of GLfloat;
    t: array[0..3] of array [0..1] of GLfloat;
  end;
  Pgl_strip_coords_t = ^gl_strip_coords_t;

var
  gld_drawinfo: GLDrawInfo;

// this is the list for all sectors to the loops
  sectorloops: PGLSectorArray;

  rendermarker: byte = 0;
  sectorrendered: PByteArray; // true if sector rendered (only here for malloc)
  csectorrenderedflatex: PByteArray;
  fsectorrenderedflatex: PByteArray;
  sectorrenderedflatex2: PByteArray;
  segrendered: PByteArray; // true if sector rendered (only here for malloc)


{*****************************
 *
 * FLATS
 *
 *****************************}

{* proff - 05/15/2000
 * The idea and algorithm to compute the flats with nodes and subsectors is
 * originaly from JHexen. I have redone it.
 *}

const
  MAX_CC_SIDES = 64;

function FIX2DBL(const x: fixed_t): double;
begin
  result := x / 1.0;
end;

function gld_PointOnSide(p: Pvertex_t; d: Pdivline_t): boolean;
begin
  // We'll return false if the point c is on the left side.
  result := (FIX2DBL(d.y) - FIX2DBL(p.y)) * FIX2DBL(d.dx) - (FIX2DBL(d.x) - FIX2DBL(p.x)) * FIX2DBL(d.dy) >= 0;
end;

// Lines start-end and fdiv must intersect.
procedure gld_CalcIntersectionVertex(s: Pvertex_t; e: Pvertex_t; d: Pdivline_t; i: Pvertex_t);
var
  ax: double;
  ay: double;
  bx: double;
  by: double;
  cx: double;
  cy: double;
  dx: double;
  dy: double;
  r: double;
begin
  ax := FIX2DBL(s.x);
  ay := FIX2DBL(s.y);
  bx := FIX2DBL(e.x);
  by := FIX2DBL(e.y);
  cx := FIX2DBL(d.x);
  cy := FIX2DBL(d.y);
  dx := cx + FIX2DBL(d.dx);
  dy := cy + FIX2DBL(d.dy);
  r := ((ay - cy) * (dx - cx) - (ax - cx) * (dy - cy)) / ((bx - ax) * (dy - cy) - (by - ay) * (dx - cx));
  i.x := round(FIX2DBL(s.x) + r * (FIX2DBL(e.x) - FIX2DBL(s.x)));
  i.y := round(FIX2DBL(s.y) + r * (FIX2DBL(e.y) - FIX2DBL(s.y)));
end;

// Returns a pointer to the list of points. It must be used.
//
function gld_FlatEdgeClipper(numpoints: Pinteger; points: Pvertex_tArray; numclippers: integer; clippers: Pdivline_tArray): Pvertex_tArray;
var
  sidelist: array[0..MAX_CC_SIDES - 1] of boolean;
  i, k, num: integer;
  curclip: Pdivline_t;
  startIdx, endIdx: integer;
  newvert: vertex_t;
  previdx: integer;
begin
  num := numpoints^;
  // We'll clip the polygon with each of the divlines. The left side of
  // each divline is discarded.
  for i := 0 to numclippers - 1 do
  begin
    curclip := @clippers[i];

    // First we'll determine the side of each vertex. Points are allowed
    // to be on the line.
    for k := 0 to num - 1 do
      sidelist[k] := gld_PointOnSide(@points[k], curclip);

    k := 0;
    while k < num do
    begin
      startIdx := k;
      endIdx := k + 1;
      // Check the end index.
      if endIdx = num then
        endIdx := 0; // Wrap-around.
      // Clipping will happen when the ends are on different sides.
      if sidelist[startIdx] <> sidelist[endIdx] then
      begin
        gld_CalcIntersectionVertex(@points[startIdx], @points[endIdx], curclip, @newvert);

        // Add the new vertex. Also modify the sidelist.
        inc(num);
        realloc(pointer(points), (num - 1) * SizeOf(vertex_t), num * SizeOf(vertex_t));
        if num >= MAX_CC_SIDES then
          I_Error('gld_FlatEdgeClipper(): Too many points in carver');

        // Make room for the new vertex.
        memmove(@points[endIdx + 1], @points[endIdx], (num - endIdx - 1) * SizeOf(vertex_t)); // VJ SOS
        memcpy(@points[endIdx], @newvert, SizeOf(newvert));

        memmove(@sidelist[endIdx + 1], @sidelist[endIdx], num - endIdx - 1);
        sidelist[endIdx] := true;

        // Skip over the new vertex.
        inc(k);
      end;
      inc(k);
    end;

    // Now we must discard the points that are on the wrong side.
    k := 0;
    while k < num do
    begin
      if not sidelist[k] then
      begin
        memmove(@points[k], @points[k + 1], (num - k - 1) * SizeOf(vertex_t));
        memmove(@sidelist[k], @sidelist[k + 1], num - k - 1);
        dec(num);
      end
      else
        inc(k);
    end;
  end;

  // Screen out consecutive identical points.
  i := 0;
  while i < num do
  begin
    previdx := i - 1;
    if previdx < 0 then
      previdx := num - 1;
    if (points[i].x = points[previdx].x) and (points[i].y = points[previdx].y) then
    begin
      // This point (i) must be removed.
      memmove(@points[i], @points[i + 1], (num - i - 1) * SizeOf(vertex_t));
      dec(num)
    end
    else
      inc(i);
  end;

  numpoints^ := num;
  result := points;
end;

procedure gld_FlatConvexCarver(ssidx: integer; num: integer; list: Pdivline_tArray);
var
  ssec: Psubsector_t;
  numclippers: integer;
  clippers: Pdivline_tArray;
  i, numedgepoints: integer;
  edgepoints: Pvertex_tArray;
  epoint: Pvertex_t;
  glsec: PGLSector;
  seg: Pseg_t;
  currentsector: integer;
  plist: Pdivline_t;
  ploop: PGLLoopDef;
  vert: PGLVertex;
  snum: integer;
begin
  ssec := @subsectors[ssidx];
  snum := ssec.numlines;
  numclippers := num + snum;

  clippers := malloc(numclippers * SizeOf(divline_t));
  for i := 0 to num - 1 do
  begin
    plist := @list[num - i - 1];
    clippers[i].x := plist.x;
    clippers[i].y := plist.y;
    clippers[i].dx := plist.dx;
    clippers[i].dy := plist.dy;
  end;
  for i := num to numclippers - 1 do
  begin
    snum := ssec.firstline;
    seg := @segs[snum + i - num];
    clippers[i].x := seg.v1.x;
    clippers[i].y := seg.v1.y;
    clippers[i].dx := seg.v2.x - seg.v1.x;
    clippers[i].dy := seg.v2.y - seg.v1.y;
  end;

  // Setup the 'worldwide' polygon.
  numedgepoints := 4;
  edgepoints := malloc(numedgepoints * Sizeof(vertex_t));

  edgepoints[0].x := MININT;
  edgepoints[0].y := MAXINT;

  edgepoints[1].x := MAXINT;
  edgepoints[1].y := MAXINT;

  edgepoints[2].x := MAXINT;
  edgepoints[2].y := MININT;

  edgepoints[3].x := MININT;
  edgepoints[3].y := MININT;

  // Do some clipping, <snip> <snip>
  edgepoints := gld_FlatEdgeClipper(@numedgepoints, edgepoints, numclippers, clippers);

  if numedgepoints >= 3 then
  begin
    gld_AddGlobalVertexes(numedgepoints);
    if (gld_vertexes <> nil) and (gld_texcoords <> nil) then
    begin

      currentsector := ssec.sector.iSectorID;

      glsec := @sectorloops[currentsector];
      glsec.loops := Z_Realloc(glsec.loops, SizeOf(GLLoopDef) * (glsec.loopcount + 1), PU_LEVEL, nil);
      ploop := @glsec.loops[glsec.loopcount];
      ploop.mode := GL_TRIANGLE_FAN;
      ploop.vertexcount := numedgepoints;
      ploop.vertexindex := gld_num_vertexes;
      inc(glsec.loopcount);

      epoint := @edgepoints[0];
      for i := 0 to numedgepoints - 1 do
      begin
        gld_texcoords[gld_num_vertexes].u := (epoint.x / FRACUNIT) / 64.0;
        gld_texcoords[gld_num_vertexes].v := (-epoint.y / FRACUNIT) / 64.0;
        vert := @gld_vertexes[gld_num_vertexes];
        vert.x := -epoint.x / MAP_SCALE;
        vert.y := 0.0;
        vert.z := epoint.y / MAP_SCALE;
        inc(gld_num_vertexes);
        inc(epoint);
      end;
    end;
  end;
  // We're done, free the edgepoints memory.
  memfree(pointer(edgepoints), numedgepoints * SizeOf(vertex_t));
  memfree(pointer(clippers), numclippers * SizeOf(divline_t));
end;

procedure gld_CarveFlats(bspnode: integer; numdivlines: integer; divlines: Pdivline_tArray; sectorclosed: PBooleanArray);
var
  nod: Pnode_t;
  dl: Pdivline_t;
  childlist: Pdivline_tArray;
  childlistsize: integer;
  ssidx: integer;
begin
  childlistsize := numdivlines + 1;

  if bspnode = -1 then
  begin
    if not sectorclosed[subsectors[0].sector.iSectorID] then
      gld_FlatConvexCarver(0, numdivlines, divlines);
    exit;
  end;

  // If this is a subsector we are dealing with, begin carving with the
  // given list.
  if bspnode and NF_SUBSECTOR <> 0 then
  begin
    // We have arrived at a subsector. The divline list contains all
    // the partition lines that carve out the subsector.
    ssidx := bspnode and not NF_SUBSECTOR;
    if not sectorclosed[subsectors[ssidx].sector.iSectorID] then
      gld_FlatConvexCarver(ssidx, numdivlines, divlines);
    exit;
  end;

  // Get a pointer to the node.
  nod := @nodes[bspnode];

  // Allocate a new list for each child.
  childlist := malloc(childlistsize * SizeOf(divline_t));

  // Copy the previous lines.
  if divlines <> nil then
    memcpy(childlist, divlines, numdivlines * SizeOf(divline_t));

  dl := @childlist[numdivlines];
  dl.x := nod.x;
  dl.y := nod.y;
  // The right child gets the original line (LEFT side clipped).
  dl.dx := nod.dx;
  dl.dy := nod.dy;
  gld_CarveFlats(nod.children[0], childlistsize, childlist, sectorclosed);

  // The left side. We must reverse the line, otherwise the wrong
  // side would get clipped.
  dl.dx := -nod.dx;
  dl.dy := -nod.dy;
  gld_CarveFlats(nod.children[1], childlistsize, childlist, sectorclosed);

  // We are finishing with this node, free the allocated list.
  memfree(pointer(childlist), childlistsize * SizeOf(divline_t));
end;


(********************************************
 * Name     : gld_GetSubSectorVertices      *
 * created  : 08/13/00                      *
 * modified : 09/18/00, adapted for PrBoom  *
 * author   : figgi                         *
 * what     : prepares subsectorvertices    *
 *            (glnodes only)                *
 ********************************************)

procedure gld_GetSubSectorVertices(sectorclosed: PBooleanArray);
var
  i, j: integer;
  numedgepoints: integer;
  ssector: Psubsector_t;
  currentsector: integer;
  glsec: PGLSector;
  seg: Pseg_t;
  ploop: PGLLoopDef;
  vert: PGLVertex;
begin
  for i := 0 to numsubsectors - 1 do
  begin
    ssector := @subsectors[i];

    if sectorclosed[ssector.sector.iSectorID] then
      continue;

    numedgepoints := ssector.numlines;
    if numedgepoints < 3 then
      continue;

    gld_AddGlobalVertexes(numedgepoints);

    if (gld_vertexes <> nil) and (gld_texcoords <> nil) then
    begin
      currentsector := ssector.sector.iSectorID;

      glsec := @sectorloops[currentsector];
      glsec.loops := Z_Realloc(glsec.loops, SizeOf(GLLoopDef) * (glsec.loopcount + 1), PU_LEVEL, nil);
      ploop := @glsec.loops[glsec.loopcount];
      ploop.mode := GL_TRIANGLE_FAN;
      ploop.vertexcount := numedgepoints;
      ploop.vertexindex := gld_num_vertexes;
      inc(glsec.loopcount);
      seg := @segs[ssector.firstline];
      for j := 0 to numedgepoints - 1 do
      begin
        gld_texcoords[gld_num_vertexes].u := (seg.v1.x / FRACUNIT) / 64.0;
        gld_texcoords[gld_num_vertexes].v :=(-seg.v1.y / FRACUNIT) / 64.0;
        vert := @gld_vertexes[gld_num_vertexes];
        vert.x := -seg.v1.x / MAP_SCALE;
        vert.y := 0.0;
        vert.z := seg.v1.y / MAP_SCALE;
        inc(gld_num_vertexes);
        inc(seg);
      end;
    end;
  end;
end;

procedure gld_PrepareSectorSpecialEffects(const sec: Psector_t);
var
  i: integer;
  line: Pline_t;
begin
  // the following is for specialeffects. see r_bsp.c in R_Subsector
  sec.no_toptextures := true;
  sec.no_bottomtextures := true;

  for i := 0 to sec.linecount - 1 do
  begin
    line := sec.lines[i];
    if (line.sidenum[0] <> -1) and
       (line.sidenum[1] <> -1) then
    begin
      if sides[line.sidenum[0]].toptexture <> 0 then
        sec.no_toptextures := false;
      if sides[line.sidenum[0]].bottomtexture <> 0 then
        sec.no_bottomtextures := false;
      if sides[line.sidenum[1]].toptexture <> 0 then
        sec.no_toptextures := false;
      if sides[line.sidenum[1]].bottomtexture <> 0 then
        sec.no_bottomtextures := false;
    end
    else
    begin
      sec.no_toptextures := false;
      sec.no_bottomtextures := false;
      exit;
    end;
  end;
end;

// gld_PreprocessLevel
//
// this checks all sectors if they are closed and calls gld_PrecalculateSector to
// calculate the loops for every sector
// the idea to check for closed sectors is from DEU. check next commentary
(*
      Note from RQ:
      This is a very simple idea, but it works!  The first test (above)
      checks that all Sectors are closed.  But if a closed set of LineDefs
      is moved out of a Sector and has all its 'external' SideDefs pointing
      to that Sector instead of the new one, then we need a second test.
      That's why I check if the SideDefs facing each other are bound to
      the same Sector.

      Other note from RQ:
      Nowadays, what makes the power of a good editor is its automatic tests.
      So, if you are writing another Doom editor, you will probably want
      to do the same kind of tests in your program.  Fine, but if you use
      these ideas, don't forget to credit DEU...  Just a reminder... :-)
*)
// so I credited DEU

procedure gld_PreprocessSectors;
var
  sectorclosed: PBooleanArray;
  vcheck1, vcheck2: PIntegerArray;
  i, j: integer;
  v1num, v2num: integer;
  ppl: PPline_t;
begin
  sectorclosed := Z_Malloc2(numsectors * SizeOf(boolean), PU_LEVEL, nil);
  if sectorclosed = nil then
    I_Error('gld_PreprocessSectors(): Not enough memory for array sectorclosed');
  ZeroMemory(sectorclosed, SizeOf(boolean) * numsectors);

  sectorloops := Z_Malloc2(numsectors * SizeOf(GLSector), PU_LEVEL, nil);
  if sectorloops = nil then
    I_Error('gld_PreprocessSectors(): Not enough memory for array sectorloops');
  ZeroMemory(sectorloops, numsectors * SizeOf(GLSector));

  sectorrendered := Z_Malloc2(numsectors * SizeOf(byte), PU_LEVEL, nil);
  if sectorrendered = nil then
    I_Error('gld_PreprocessSectors(): Not enough memory for array sectorrendered');
  ZeroMemory(sectorrendered, numsectors * SizeOf(byte));

  csectorrenderedflatex := Z_Malloc2(numsectors * SizeOf(byte), PU_LEVEL, nil);
  if csectorrenderedflatex = nil then
    I_Error('gld_PreprocessSectors(): Not enough memory for array csectorrenderedflatex');
  ZeroMemory(csectorrenderedflatex, numsectors * SizeOf(byte));

  fsectorrenderedflatex := Z_Malloc2(numsectors * SizeOf(byte), PU_LEVEL, nil);
  if fsectorrenderedflatex = nil then
    I_Error('gld_PreprocessSectors(): Not enough memory for array fsectorrenderedflatex');
  ZeroMemory(fsectorrenderedflatex, numsectors * SizeOf(byte));

  sectorrenderedflatex2 := Z_Malloc2(numsectors * SizeOf(byte), PU_LEVEL, nil);
  if sectorrenderedflatex2 = nil then
    I_Error('gld_PreprocessSectors(): Not enough memory for array sectorrenderedflatex2');
  ZeroMemory(sectorrenderedflatex2, numsectors * SizeOf(byte));

  segrendered := Z_Malloc2(numsegs * SizeOf(byte), PU_LEVEL, nil);
  if segrendered = nil then
    I_Error('gld_PreprocessSectors(): Not enough memory for array segrendered');
  ZeroMemory(segrendered, numsegs * SizeOf(byte));

  gld_vertexes := nil;
  gld_texcoords := nil;
  gld_max_vertexes := 0;
  gld_num_vertexes := 0;
  gld_AddGlobalVertexes(numvertexes * 2);

  // JVAL: From prboom-plus
  vcheck1 := malloc(numvertexes * SizeOf(vcheck1[0]));
  vcheck2 := malloc(numvertexes * SizeOf(vcheck2[0]));
  for i := 0 to numsectors - 1 do
  begin
    gld_PrepareSectorSpecialEffects(@sectors[i]);

    ZeroMemory(vcheck1, numvertexes * SizeOf(vcheck1[0]));
    ZeroMemory(vcheck2, numvertexes * SizeOf(vcheck2[0]));

    ppl := @sectors[i].lines[0];
    for j := 0 to sectors[i].linecount - 1 do
    begin
      v1num := (integer(ppl^.v1) - integer(vertexes)) div SizeOf(vertex_t);
      v2num := (integer(ppl^.v2) - integer(vertexes)) div SizeOf(vertex_t);
      if (v1num >= numvertexes) or (v2num >= numvertexes) then
        continue;

      // e6y: for correct handling of missing textures.
      // We do not need to apply some algos for isolated lines.
      inc(vcheck2[v1num]);
      inc(vcheck2[v2num]);

      if ppl^.sidenum[0] <> - 1 then
        if sides[ppl^.sidenum[0]].sector = @sectors[i] then
        begin
          vcheck1[v1num] := vcheck1[v1num] or 1;
          vcheck1[v2num] := vcheck1[v2num] or 2;
        end;
      if ppl^.sidenum[1] <> -1 then
        if sides[ppl^.sidenum[1]].sector = @sectors[i] then
        begin
          vcheck1[v1num] := vcheck1[v1num] or 2;
          vcheck1[v2num] := vcheck1[v2num] or 1;
        end;
      inc(ppl);
    end;

    ppl := @sectors[i].lines[0];
    for j := 0 to sectors[i].linecount - 1 do
    begin
      v1num :=(integer(ppl^.v1) -integer(vertexes)) div SizeOf(vertex_t);
      v2num :=(integer(ppl^.v2) -integer(vertexes)) div SizeOf(vertex_t);
      if (vcheck2[v1num] < 2) and (vcheck2[v2num] < 2) then
        ppl^.renderflags := ppl^.renderflags or LRF_ISOLATED;
      inc(ppl);
    end;
  end;
  memfree(pointer(vcheck1), numvertexes * SizeOf(vcheck1[0]));
  memfree(pointer(vcheck2), numvertexes * SizeOf(vcheck2[0]));

  // figgi -- adapted for glnodes // JVAL
  if glnodesver = 0 then
    gld_CarveFlats(numnodes - 1, 0, nil, sectorclosed)
  else
    gld_GetSubSectorVertices(sectorclosed);

  Z_Free(sectorclosed);
end;

var
  roll: float = 0.0;
  yaw: float = 0.0;
  inv_yaw: float = 0.0;
  pitch: float = 0.0;

procedure infinitePerspective(fovy: GLdouble; aspect: GLdouble; znear: GLdouble);
var
  left, right, bottom, top: GLdouble;
  m: array[0..15] of GLdouble;
begin
  top := znear * tan(fovy * __glPi / 360.0);
  bottom := -top;
  left := bottom * aspect;
  right := top * aspect;

  m[ 0] := (2 * znear) / (right - left);
  m[ 4] := 0;
  m[ 8] := (right + left) / (right - left);
  m[12] := 0;

  m[ 1] := 0;
  m[ 5] := (2 * znear) / (top - bottom);
  m[ 9] := (top + bottom) / (top - bottom);
  m[13] := 0;

  m[ 2] := 0;
  m[ 6] := 0;
//  m[10] := - (zfar + znear) / (zfar - znear);
//  m[14] := - (2 * zfar * znear) / (zfar - znear);
  m[10] := -1;
  m[14] := -2 * znear;

  m[ 3] := 0;
  m[ 7] := 0;
  m[11] := -1;
  m[15] := 0;

  glMultMatrixd(@m);
end;

var
  dodrawsky: boolean;
  didfirstscreensync: boolean = false;

var
  xCamera, yCamera, zCamera: float;

procedure gld_StartDrawScene;
var
  height: integer;
  syncret: boolean;
begin
  if gl_shared_texture_palette then
    glEnable(GL_SHARED_TEXTURE_PALETTE_EXT);
  gld_SetPalette(-1);

  if (last_screensync <> gl_screensync) or not didfirstscreensync then
  begin
    if gl_screensync then
    begin
      syncret := gld_VSync(vsmSync);
      if not didfirstscreensync then
      begin
        gld_VSync(vsmNoSync);
        gld_VSync(vsmSync);
      end;
    end
    else
    begin
      syncret := gld_VSync(vsmNoSync);
      if not didfirstscreensync then
      begin
        gld_VSync(vsmSync);
        gld_VSync(vsmNoSync);
      end;
    end;
    last_screensync := gl_screensync;
    didfirstscreensync := syncret;
  end;

  if screenblocks > 10 then
    height := SCREENHEIGHT
  else if screenblocks = 10 then
    height := SCREENHEIGHT
  else
    height := (screenblocks * SCREENHEIGHT div 10) and not 7;

  glViewport(viewwindowx, SCREENHEIGHT - (height + viewwindowy - ((height - viewheight) div 2)), viewwidth, height);
  if screenblocks > 10 then
  begin
    glDisable(GL_SCISSOR_TEST);
    scissoron := false;
  end
  else
  begin
    glScissor(viewwindowx, SCREENHEIGHT - (viewheight + viewwindowy), viewwidth, viewheight);
    glEnable(GL_SCISSOR_TEST);
    scissoron := true;
  end;

  // Player coordinates
  xCamera := -viewx / MAP_SCALE;
  yCamera := viewy / MAP_SCALE;
  zCamera := viewz / MAP_SCALE;

  yaw := 270.0 - (viewangle shr ANGLETOFINESHIFT) * 360.0 / FINEANGLES;
  inv_yaw := -90.0 + (viewangle shr ANGLETOFINESHIFT) * 360.0 / FINEANGLES;

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  glShadeModel(GL_SMOOTH);

  glEnable(GL_DEPTH_TEST);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  infinitePerspective(64.0, 320.0 / 200.0 * monitor_relative_aspect, gl_nearclip / 100.0);


  if zaxisshift then
    pitch := -players[displayplayer].lookdir16 / 32 // JVAL Smooth Look Up/Down
  else
    pitch := 0;
  // JVAL: Correct 2d bsp limitation
  if pitch > 45 then
    pitch := 45;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glRotatef(roll,  0.0, 0.0, 1.0);
  glRotatef(pitch, 1.0, 0.0, 0.0);
  glRotatef(yaw,   0.0, 1.0, 0.0);
  glTranslatef(-xCamera, -zCamera, -yCamera);
  camera.rotation[0] := pitch;
  camera.rotation[1] := yaw;
  camera.rotation[2] := roll;
  camera.position[0] := xCamera;
  camera.position[1] := zCamera;
  camera.position[2] := yCamera;

  inc(rendermarker);
  gld_drawinfo.num_walls := 0;
  gld_drawinfo.num_flats := 0;
  gld_drawinfo.num_sprites := 0;
  gld_drawinfo.num_drawitems := 0;
  dodrawsky := false;
  numdlitems := 0;

  fr_CalculateFrustum;

  if gl_renderwireframe then
  begin
    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glClearDepth(1.0);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  end
  else
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
end;

procedure gld_EndDrawScene;
var
  player: Pplayer_t;
begin
  player := @players[displayplayer];

  glDisable(GL_POLYGON_SMOOTH);

  glViewport(0, 0, SCREENWIDTH, SCREENHEIGHT);
  gld_Set2DMode;

 // gld_AmbientExecute;

  R_DrawPlayer;

  if scissoron then
    glDisable(GL_SCISSOR_TEST);

  gld_DrawBackgroundFrame;

  if scissoron then
    glEnable(GL_SCISSOR_TEST);

  if player.fixedcolormap = 32 then
  begin
    glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ZERO);
    glColor4f(1.0, 1.0, 1.0, 1.0);
    glBindTexture(GL_TEXTURE_2D, 0);
    last_gltexture := nil;
    last_cm := -1;
    glBegin(GL_TRIANGLE_STRIP);
      glVertex2f(0.0, 0.0);
      glVertex2f(0.0, SCREENHEIGHT);
      glVertex2f(SCREENWIDTH, 0.0);
      glVertex2f(SCREENWIDTH, SCREENHEIGHT);
    glEnd;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glDisable(GL_ALPHA_TEST);
    glColor4f(0.3, 0.3, 0.3, 0.3);
    glBindTexture(GL_TEXTURE_2D, 0);
    last_gltexture := nil;
    last_cm := -1;
    glBegin(GL_TRIANGLE_STRIP);
      glVertex2f(0.0, 0.0);
      glVertex2f(0.0, SCREENHEIGHT);
      glVertex2f(SCREENWIDTH, 0.0);
      glVertex2f(SCREENWIDTH, SCREENHEIGHT);
    glEnd;
    glEnable(GL_ALPHA_TEST);
  end;

  if gl_renderwireframe then
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
  begin
    extra_red := extra_red + customcolormap.fog_r;
    if extra_red > 1.0 then
      extra_red := 1.0;
    extra_green := extra_green + customcolormap.fog_g;
    if extra_green > 1.0 then
      extra_green := 1.0;
    extra_blue := extra_blue + customcolormap.fog_b;
    if extra_blue > 1.0 then
      extra_blue := 1.0;
    extra_alpha := extra_alpha + 0.8;
    if extra_alpha > 1.0 then
      extra_alpha := 1.0;
  end;
  {$ENDIF}

  if extra_alpha > 0.0 then
  begin
    glDepthMask(False);
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_ALPHA_TEST);
    glBlendFunc(GL_ONE, GL_ONE);

    glColor4f(extra_red * 0.25, extra_green * 0.25, extra_blue * 0.25, extra_alpha);
    glBindTexture(GL_TEXTURE_2D, 0);
    last_gltexture := nil;
    last_cm := -1;
    glBegin(GL_TRIANGLE_STRIP);
      glVertex2f(0.0, 0.0);
      glVertex2f(0.0, SCREENHEIGHT);
      glVertex2f(SCREENWIDTH, 0.0);
      glVertex2f(SCREENWIDTH, SCREENHEIGHT);
    glEnd;

    glDepthMask(True);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_ALPHA_TEST);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  end;

  glColor3f(1.0, 1.0, 1.0);
  glDisable(GL_SCISSOR_TEST);
  scissoron := false;
  if gl_shared_texture_palette then
    glDisable(GL_SHARED_TEXTURE_PALETTE_EXT);
end;

procedure gld_AddDrawItem(itemtype: GLDrawItemType; itemindex: integer);
var
  item: PGLDrawItem;
begin
  if gld_drawinfo.num_drawitems >= gld_drawinfo.max_drawitems then
  begin
    gld_drawinfo.max_drawitems := gld_drawinfo.max_drawitems + 64;
    gld_drawinfo.drawitems := Z_Realloc(gld_drawinfo.drawitems, gld_drawinfo.max_drawitems * SizeOf(GLDrawItem), PU_LEVEL, nil);
    item := @gld_drawinfo.drawitems[gld_drawinfo.num_drawitems];
    item.itemtype := itemtype;
    item.itemcount := 1;
    item.firstitemindex := itemindex;
    item.rendermarker := rendermarker;
    exit;
  end;
  item := @gld_drawinfo.drawitems[gld_drawinfo.num_drawitems];
  if item.rendermarker <> rendermarker then
  begin
    item.itemtype := GLDIT_NONE;
    item.rendermarker := rendermarker;
  end;
  if item.itemtype <> itemtype then
  begin
    if item.itemtype <> GLDIT_NONE then
    begin
      inc(gld_drawinfo.num_drawitems);
    end;
    if gld_drawinfo.num_drawitems >= gld_drawinfo.max_drawitems then
    begin
      gld_drawinfo.max_drawitems := gld_drawinfo.max_drawitems + 64;
      gld_drawinfo.drawitems := Z_Realloc(gld_drawinfo.drawitems, gld_drawinfo.max_drawitems * SizeOf(GLDrawItem), PU_LEVEL, nil);
    end;
    item := @gld_drawinfo.drawitems[gld_drawinfo.num_drawitems];
    item.itemtype := itemtype;
    item.itemcount := 1;
    item.firstitemindex := itemindex;
    item.rendermarker := rendermarker;
    exit;
  end;
  inc(item.itemcount);
end;

(*****************
 *               *
 * Walls         *
 *               *
 *****************)

const
  SMALLDELTA = 0.001;

procedure CALC_Y_VALUES(w: PGLWall; var lineheight: float; floor_height, ceiling_height: integer);
begin
  w.ytop := ceiling_height / MAP_SCALE + SMALLDELTA;
  w.ybottom := floor_height / MAP_SCALE - SMALLDELTA;
  lineheight := abs((ceiling_height - floor_height) / FRACUNIT);
end;

procedure CALC_Y_VALUES2(w: PGLWall; var lineheight: float; floor_height, ceiling_height: integer);
begin
  w.ytop := ceiling_height / MAP_SCALE + SMALLDELTA;
  w.ybottom := floor_height / MAP_SCALE - SMALLDELTA;
  lineheight := (ceiling_height - floor_height) / FRACUNIT;
end;

function OU(tex: PGLTexture; seg: Pseg_t): float;
begin
  result := ((seg.sidedef.textureoffset + seg.offset) / FRACUNIT) / tex.buffer_width;
end;

function OV(tex: PGLTexture; seg: Pseg_t): float;
begin
  result := (seg.sidedef.rowoffset / FRACUNIT) / tex.buffer_height;
end;

function OV_PEG(tex: PGLTexture; seg: Pseg_t; v_offset: integer): float;
begin
  result := ((seg.sidedef.rowoffset - v_offset) / FRACUNIT) / tex.buffer_height;
end;

procedure CALC_TEX_VALUES_TOP(w: PGLWall; seg: Pseg_t; peg: boolean; linelength, lineheight: float);
var
  tex: PGLTexture;
begin
  w.flag := GLDWF_TOP;
  tex := w.gltexture;
  w.ul := OU(tex, seg);
  w.ur := w.ul + (linelength / tex.buffer_width);
  if peg then
  begin
    w.vb := OV(tex, seg) + (tex.height / tex.tex_height);
    w.vt := w.vb - (lineheight / tex.buffer_height);
  end
  else
  begin
    w.vt := OV(tex, seg);
    w.vb := w.vt + (lineheight / tex.buffer_height);
  end;
end;

procedure CALC_TEX_VALUES_MIDDLE1S(w: PGLWall; seg: Pseg_t; peg: boolean; linelength, lineheight: float);
var
  tex: PGLTexture;
begin
  w.flag := GLDWF_M1S;
  tex := w.gltexture;
  w.ul := OU(tex, seg);
  w.ur := w.ul + (linelength / tex.buffer_width);
  if peg then
  begin
    w.vb := OV(tex, seg) + tex.heightscale;
    w.vt := w.vb - (lineheight / tex.buffer_height);
  end
  else
  begin
    w.vt := OV(tex, seg);
    w.vb := w.vt + (lineheight / tex.buffer_height);
  end;
end;

procedure CALC_TEX_VALUES_MIDDLETS(w: PGLWall; seg: Pseg_t; peg: boolean; linelength, lineheight: float);
var
  tex: PGLTexture;
begin
  w.flag := GLDWF_MTS;
  tex := w.gltexture;
  w.ul := OU(tex, seg);
  w.ur := w.ul + (linelength / tex.buffer_width);
  if peg then
  begin
    w.vb := OV(tex, seg) + tex.heightscale;
    w.vt := w.vb - (lineheight / tex.buffer_height);
  end
  else
  begin
    w.vt := OV(tex, seg);
    w.vb := w.vt + (lineheight / tex.buffer_height);
  end;
end;

procedure CALC_TEX_VALUES_MIDDLE2S(w: PGLWall; seg: Pseg_t; peg: boolean; linelength, lineheight: float);
var
  tex: PGLTexture;
begin
  w.flag := GLDWF_M2S;
  tex := w.gltexture;
  w.ul := OU(tex, seg);
  w.ur := w.ul + (linelength / tex.buffer_width);
  if peg then
  begin
    w.vb := tex.heightscale;
    w.vt := w.vb - (lineheight / tex.buffer_height)
  end
  else
  begin
    w.vt := 0.0;
    w.vb := lineheight / tex.buffer_height;
  end;
end;

procedure CALC_TEX_VALUES_BOTTOM(w: PGLWall; seg: Pseg_t; peg: boolean; linelength, lineheight: float; v_offset: integer);
var
  tex: PGLTexture;
begin
  w.flag := GLDWF_BOT;
  tex := w.gltexture;
  w.ul := OU(tex, seg);
  w.ur := w.ul + (linelength / tex.realtexwidth);
  if peg then
  begin
    w.vb := OV_PEG(tex, seg, v_offset) + tex.heightscale;
    w.vt := w.vb - lineheight / tex.buffer_height;
  end
  else
  begin
    w.vt := OV(tex, seg);
    w.vb := w.vt + lineheight / tex.buffer_height;
  end;
end;

{$IFDEF DOOM}
function gld_GetSkyTexture(const w: PGLWall): integer;
begin
  result := skytexture;
  if w.glseg.frontsector.sky and PL_SKYFLAT <> 0 then
    result := texturetranslation[sides[lines[frontsector.sky and not PL_SKYFLAT].sidenum[0]].toptexture];
end;

function gld_GetSkyTextureFlag(const w: PGLWall): integer;
begin
  result := GLDWF_SKY;
  if w.glseg.frontsector.sky and PL_SKYFLAT <> 0 then
    if lines[frontsector.sky and not PL_SKYFLAT].special = 271 then
      result := GLDWF_SKYFLIP;
end;
{$ENDIF}

procedure ADDSKYTEXTURE(wall: PGLWall);
begin
  wall.gltexture := gld_RegisterTexture({$IFDEF DOOM}gld_GetSkyTexture(wall){$ELSE}skytexture{$ENDIF}, false);
  wall.gltexture.textype := GLDT_SKY;
  wall.skyyaw := -2.0 * ((yaw + 90.0) / 90.0);
  wall.skyymid := 200.0 / 319.5;
  wall.flag := {$IFDEF DOOOM}gld_GetSkyTextureFlag(wall){$ELSE}GLDWF_SKY{$ENDIF};
  dodrawsky := true;
end;

procedure ADDWALL(wall: PGLWall);
begin
  if gld_drawinfo.num_walls >= gld_drawinfo.max_walls then
  begin
    gld_drawinfo.max_walls := gld_drawinfo.max_walls + 128;
    gld_drawinfo.walls := Z_Realloc(gld_drawinfo.walls, gld_drawinfo.max_walls * SizeOf(GLWall), PU_LEVEL, nil);
  end;
  wall.blend := not ((wall.flag in [GLDWF_SKY, GLDWF_SKYFLIP, GLDWF_TOP, GLDWF_M1S, GLDWF_BOT]) or (wall.alpha > 0.999));
  gld_AddDrawItem(GLDIT_WALL, gld_drawinfo.num_walls);
  gld_drawinfo.walls[gld_drawinfo.num_walls] := wall^;
  inc(gld_drawinfo.num_walls);
end;

//==========================================================================
//
// Flood gaps with the back side's ceiling/floor texture
// This requires a stencil because the projected plane interferes with
// the depth buffer
//
//==========================================================================

procedure gld_SetupFloodStencil(const wall: PGLWall);
var
  recursion: integer;
begin
  recursion := 0;

  // Create stencil
  glStencilFunc(GL_EQUAL, recursion, $FFFFFFFF);  // create stencil
  glStencilOp(GL_KEEP, GL_KEEP, GL_INCR); // increment stencil of valid pixels
  glColorMask(false, false, false, false); // don't write to the graphics buffer
  glDisable(GL_TEXTURE_2D);
  glColor3f(1, 1, 1);
  glEnable(GL_DEPTH_TEST);
  glDepthMask(true);

  glBegin(GL_TRIANGLE_FAN);
    glVertex3f(wall.glseg.x1, wall.ytop, wall.glseg.z1);
    glVertex3f(wall.glseg.x1, wall.ybottom, wall.glseg.z1);
    glVertex3f(wall.glseg.x2, wall.ybottom, wall.glseg.z2);
    glVertex3f(wall.glseg.x2, wall.ytop, wall.glseg.z2);
  glEnd();

  glStencilFunc(GL_EQUAL, recursion + 1, $FFFFFFFF); // draw sky into stencil
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);   // this stage doesn't modify the stencil

  glColorMask(true, true, true, true); // don't write to the graphics buffer
  glEnable(GL_TEXTURE_2D);
//  glDisable(GL_DEPTH_TEST);
  glDepthMask(false);
end;

procedure gld_ClearFloodStencil(const wall: PGLWall);
var
  recursion: integer;
begin
  recursion := 0;

  glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);
  glDisable(GL_TEXTURE_2D);
  glColorMask(false, false, false, false); // don't write to the graphics buffer
  glColor3f(1, 1, 1);

  glBegin(GL_TRIANGLE_FAN);
    glVertex3f(wall.glseg.x1, wall.ytop, wall.glseg.z1);
    glVertex3f(wall.glseg.x1, wall.ybottom, wall.glseg.z1);
    glVertex3f(wall.glseg.x2, wall.ybottom, wall.glseg.z2);
    glVertex3f(wall.glseg.x2, wall.ytop, wall.glseg.z2);
  glEnd();

  // restore old stencil op.
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glStencilFunc(GL_EQUAL, recursion, $FFFFFFFF);
  glEnable(GL_TEXTURE_2D);
  glColorMask(true, true, true, true);
  glEnable(GL_DEPTH_TEST);
  glDepthMask(true);
end;

//
// Calculation of the coordinates of the gap
//
procedure gld_SetupFloodedPlaneCoords(const wall: PGLWall; const c: Pgl_strip_coords_t);
var
  prj_fac1, prj_fac2: float;
  k: float;
  ytop, ybottom, planez: float;
begin
  if wall.flag = GLDWF_TOPFLUD then
  begin
    ytop := wall.ybottom;
    ybottom := wall.ytop;
    planez := wall.ybottom;
  end
  else
  begin
    ytop := wall.ytop;
    ybottom := wall.ybottom;
    planez := wall.ytop;
  end;

  prj_fac1 := (ytop - zCamera) / (ytop - zCamera);
  prj_fac2 := (ytop - zCamera) / (ybottom - zCamera);

  c.v[0][0] := xCamera + prj_fac1 * (wall.glseg.x1 - xCamera);
  c.v[0][1] := planez;
  c.v[0][2] := yCamera + prj_fac1 * (wall.glseg.z1 - yCamera);

  c.v[1][0] := xCamera + prj_fac2 * (wall.glseg.x1 - xCamera);
  c.v[1][1] := planez;
  c.v[1][2] := yCamera + prj_fac2 * (wall.glseg.z1 - yCamera);

  c.v[2][0] := xCamera + prj_fac1 * (wall.glseg.x2 - xCamera);
  c.v[2][1] := planez;
  c.v[2][2] := yCamera + prj_fac1 * (wall.glseg.z2 - yCamera);

  c.v[3][0] := xCamera + prj_fac2 * (wall.glseg.x2 - xCamera);
  c.v[3][1] := planez;
  c.v[3][2] := yCamera + prj_fac2 * (wall.glseg.z2 - yCamera);

  k := 0.5;

  c.t[0][0] := -c.v[0][0] / k;
  c.t[0][1] := -c.v[0][2] / k;

  c.t[1][0] := -c.v[1][0] / k;
  c.t[1][1] := -c.v[1][2] / k;

  c.t[2][0] := -c.v[2][0] / k;
  c.t[2][1] := -c.v[2][2] / k;

  c.t[3][0] := -c.v[3][0] / k;
  c.t[3][1] := -c.v[3][2] / k;
end;

procedure gld_SetupFloodedPlaneLight(const wall: PGLWall);
var
  light: float;
begin
  if wall.glseg.backsector <> nil then
  begin
    light := gld_CalcLightLevel(wall.glseg.backsector.lightlevel + (extralight shr 5));
    gld_StaticLightAlpha(light, wall.alpha);
  end
  else
    gld_StaticLightAlpha(wall.light, wall.alpha);
end;

procedure gld_DrawTriangleStrip(const c: Pgl_strip_coords_t);
begin
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(c.t[0][0], c.t[0][1]);
    glVertex3f(c.v[0][0], c.v[0][1], c.v[0][2]);
    glTexCoord2f(c.t[1][0], c.t[1][1]);
    glVertex3f(c.v[1][0], c.v[1][1], c.v[1][2]);
    glTexCoord2f(c.t[2][0], c.t[2][1]);
    glVertex3f(c.v[2][0], c.v[2][1], c.v[2][2]);
    glTexCoord2f(c.t[3][0], c.t[3][1]);
    glVertex3f(c.v[3][0], c.v[3][1], c.v[3][2]);
  glEnd();
end;

procedure gld_StartFog; forward;

procedure gld_StartWhiteFog; forward;

procedure gld_DrawWall(wall: PGLWall; const fblend: boolean);
var
  seg: PGLSeg;
  floorheight: float;
  ceilingheight: float;
  theight: float;
  frontslope: Boolean;
  backslope: Boolean;
  vm: float;
  A: array[0..3] of GLVertexUV;
  iA: integer;
  c: gl_strip_coords_t;
  didwhitefog: boolean; // JVAL: Mars fog sectors

  procedure ADD_A_COORD(const au, av: float);
  begin
    A[iA].u := au;
    A[iA].v := av;
  end;

  procedure ADD_A_XYZ(const ax, ay, az: float);
  begin
    A[iA].x := ax;
    A[iA].y := ay;
    A[iA].z := az;
  end;

  procedure RENDER_A_SIMPLE;
  var
    ii: integer;
  begin
    if iA < 3 then
      Exit;

    glBegin(GL_TRIANGLE_STRIP);
      for ii := 0 to iA - 1 do
      begin
        glTexCoord2f(A[ii].u, A[ii].v);
        glVertex3f(A[ii].x, A[ii].y, A[ii].z);
      end;
    glEnd;
  end;

  procedure RENDER_A;
  var
    ii: integer;
  begin
    if (iA < 4) or not wall.doublelight then
    begin
      RENDER_A_SIMPLE;
      Exit;
    end;

    if (wall.ymid <= A[0].y) and
       (wall.ymid >= A[1].y) and
       (wall.ymid <= A[2].y) and
       (wall.ymid >= A[3].y) then
    begin
      theight := wall.gltexture.buffer_height / MAP_COEFF;
      vm := wall.vt + (wall.ytop - wall.ymid) / theight;

      glBegin(GL_TRIANGLE_STRIP);
        glTexCoord2f(A[0].u, A[0].v);
        glVertex3f(A[0].x, A[0].y, A[0].z);
        glTexCoord2f(A[1].u, vm);
        glVertex3f(A[1].x, wall.ymid, A[1].z);
        glTexCoord2f(A[2].u, A[2].v);
        glVertex3f(A[2].x, A[2].y, A[2].z);
        glTexCoord2f(A[3].u, vm);
        glVertex3f(A[3].x, wall.ymid, A[3].z);
      glEnd;

      gld_StaticLightAlpha(wall.light2, wall.alpha);

      glBegin(GL_TRIANGLE_STRIP);
        glTexCoord2f(A[0].u, vm);
        glVertex3f(A[0].x, wall.ymid, A[0].z);
        glTexCoord2f(A[1].u, A[1].v);
        glVertex3f(A[1].x, A[1].y, A[1].z);
        glTexCoord2f(A[2].u, vm);
        glVertex3f(A[2].x, wall.ymid, A[2].z);
        glTexCoord2f(A[3].u, A[3].v);
        glVertex3f(A[3].x, A[3].y, A[3].z);

        glTexCoord2f(wall.ul, vm);
        glVertex3f(seg.x1, wall.ymid, seg.z1);
        glTexCoord2f(wall.ul, wall.vb);
        glVertex3f(seg.x1, wall.ybottom, seg.z1);
        glTexCoord2f(wall.ur, vm);
        glVertex3f(seg.x2, wall.ymid, seg.z2);
        glTexCoord2f(wall.ur, wall.vb);
        glVertex3f(seg.x2, wall.ybottom, seg.z2);
      glEnd;
    end
    else
    begin
      glBegin(GL_TRIANGLE_STRIP);
        for ii := 0 to iA - 1 do
        begin
          glTexCoord2f(A[ii].u, A[ii].v);
          glVertex3f(A[ii].x, A[ii].y, A[ii].z);
        end;
      glEnd;
    end;
  end;

  procedure WALL_WHITEFOG_1; // JVAL: Mars fog sectors
  begin
    if wall.whitefog then
    begin
      gld_StartWhiteFog;
      didwhitefog := true;
    end;
  end;

  procedure WALL_WHITEFOG_2; // JVAL: Mars fog sectors
  begin
    if wall.whitefog2 then
    begin
      gld_StartWhiteFog;
      didwhitefog := true;
    end;
  end;

  procedure WALL_WHITEFOG_STOP;  // JVAL: Mars fog sectors
  begin
    if didwhitefog then
    begin
      gld_StartFog;
      didwhitefog := false;
    end;
  end;

begin
  if not gl_drawsky and (wall.flag >= GLDWF_SKY) then
    exit;

  if wall.gltexture.index = 0 then
    exit;

  if wall.blend <> fblend then
    exit;

  didwhitefog := false;

  if (wall.flag = GLDWF_TOPFLUD) or (wall.flag = GLDWF_BOTFLUD) then
  begin
    gld_BindFlat(wall.gltexture);

    gld_SetupFloodStencil(wall);
    gld_SetupFloodedPlaneCoords(wall, @c);
    gld_SetupFloodedPlaneLight(wall);
    WALL_WHITEFOG_1; // JVAL: Mars fog sectors
    gld_DrawTriangleStrip(@c);
    WALL_WHITEFOG_STOP; // JVAL: Mars fog sectors
    gld_ClearFloodStencil(wall);

    exit;
  end;

  gld_BindTexture(wall.gltexture);
  if wall.flag >= GLDWF_SKY then
  begin
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    if wall.flag = GLDWF_SKYFLIP then
      glScalef(-128.0 / wall.gltexture.buffer_width, 200.0 / 320.0 * 2.0, 1.0)
    else
      glScalef(128.0 / wall.gltexture.buffer_width, 200.0 / 320.0 * 2.0, 1.0);
    glTranslatef(wall.skyyaw, wall.skyymid, 0.0);

    seg := wall.glseg;

    glBegin(GL_TRIANGLE_STRIP);
      glVertex3f(seg.x1, wall.ytop, seg.z1);
      glVertex3f(seg.x1, wall.ybottom, seg.z1);
      glVertex3f(seg.x2, wall.ytop, seg.z2);
      glVertex3f(seg.x2, wall.ybottom, seg.z2);
    glEnd;

    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
  end
  else
  begin
    gld_StaticLightAlpha(wall.light, wall.alpha);
    seg := wall.glseg;

    if wall.blend then
      glDepthMask(false)
    else
      glDisable(GL_BLEND);

    frontslope := seg.frontsector.renderflags and SRF_SLOPED <> 0;
    if seg.backsector = nil then
      backslope := false
    else
      backslope := seg.backsector.renderflags and SRF_SLOPED <> 0;

    if frontslope and (wall.flag = GLDWF_M1S) then
    begin
      theight := wall.gltexture.buffer_height / MAP_COEFF;

      iA := 0;

      floorheight := gld_FloorHeight(seg.frontsector, seg.x1, seg.z1);
      ceilingheight := gld_CeilingHeight(seg.frontsector, seg.x1, seg.z1);

      if ceilingheight - floorheight > GLEPSILON then
      begin
        ADD_A_COORD(wall.ul, wall.vt + (wall.ytop - ceilingheight) / theight);
        ADD_A_XYZ(seg.x1, ceilingheight, seg.z1);
        Inc(iA);
      end;

      ADD_A_COORD(wall.ul, wall.vt + (wall.ytop - floorheight) / theight);
      ADD_A_XYZ(seg.x1, floorheight, seg.z1);
      Inc(iA);

      floorheight := gld_FloorHeight(seg.frontsector, seg.x2, seg.z2);
      ceilingheight := gld_CeilingHeight(seg.frontsector, seg.x2, seg.z2);

      if ceilingheight - floorheight > GLEPSILON then
      begin
        ADD_A_COORD(wall.ur, wall.vt + (wall.ytop - ceilingheight) / theight);
        ADD_A_XYZ(seg.x2, ceilingheight, seg.z2);
        Inc(iA);
      end;

      ADD_A_COORD(wall.ur, wall.vt + (wall.ytop - floorheight) / theight);
      ADD_A_XYZ(seg.x2, floorheight, seg.z2);
      Inc(iA);

      WALL_WHITEFOG_1; // JVAL: Mars fog sectors
      RENDER_A;
      WALL_WHITEFOG_STOP; // JVAL: Mars fog sectors
    end
    else if (frontslope or backslope) and (wall.flag = GLDWF_BOT) then
    begin
      theight := wall.gltexture.buffer_height / MAP_COEFF;

      iA := 0;

      floorheight := gld_FloorHeight(seg.frontsector, seg.x1, seg.z1);
      ceilingheight := gld_FloorHeight(seg.backsector, seg.x1, seg.z1);

      if ceilingheight - floorheight > GLEPSILON then
      begin
        ADD_A_COORD(wall.ul, wall.vt + (wall.ytop - ceilingheight) / theight);
        ADD_A_XYZ(seg.x1, ceilingheight, seg.z1);
        Inc(iA);
      end;

      ADD_A_COORD(wall.ul, wall.vt + (wall.ytop - floorheight) / theight);
      ADD_A_XYZ(seg.x1, floorheight, seg.z1);
      Inc(iA);

      floorheight := gld_FloorHeight(seg.frontsector, seg.x2, seg.z2);
      ceilingheight := gld_FloorHeight(seg.backsector, seg.x2, seg.z2);

      if ceilingheight - floorheight > GLEPSILON then
      begin
        ADD_A_COORD(wall.ur, wall.vt + (wall.ytop - ceilingheight) / theight);
        ADD_A_XYZ(seg.x2, ceilingheight, seg.z2);
        Inc(iA);
      end;

      ADD_A_COORD(wall.ur, wall.vt + (wall.ytop - floorheight) / theight);
      ADD_A_XYZ(seg.x2, floorheight, seg.z2);
      Inc(iA);

      WALL_WHITEFOG_1; // JVAL: Mars fog sectors
      RENDER_A;
      WALL_WHITEFOG_STOP; // JVAL: Mars fog sectors
    end
    else if (frontslope or backslope) and (wall.flag = GLDWF_TOP) then
    begin
      theight := wall.gltexture.buffer_height / MAP_COEFF;

      WALL_WHITEFOG_1; // JVAL: Mars fog sectors
      glBegin(GL_TRIANGLE_STRIP);
        floorheight := gld_CeilingHeight(seg.frontsector, seg.x1, seg.z1);
        ceilingheight := gld_CeilingHeight(seg.backsector, seg.x1, seg.z1);

        glTexCoord2f(wall.ul, wall.vt + (wall.ytop - ceilingheight) / theight);
        glVertex3f(seg.x1, ceilingheight, seg.z1);
        glTexCoord2f(wall.ul, wall.vt + (wall.ytop - floorheight) / theight);
        glVertex3f(seg.x1, floorheight, seg.z1);

        floorheight := gld_CeilingHeight(seg.frontsector, seg.x2, seg.z2);
        ceilingheight := gld_CeilingHeight(seg.backsector, seg.x2, seg.z2);

        glTexCoord2f(wall.ur, wall.vt + (wall.ytop - ceilingheight) / theight);
        glVertex3f(seg.x2, ceilingheight, seg.z2);
        glTexCoord2f(wall.ur, wall.vt + (wall.ytop - floorheight) / theight);
        glVertex3f(seg.x2, floorheight, seg.z2);
      glEnd;
      WALL_WHITEFOG_STOP; // JVAL: Mars fog sectors
    end
    else
    begin
      if not wall.doublelight or (wall.ymid < wall.ybottom) or (wall.ymid > wall.ytop) then
      begin
        WALL_WHITEFOG_1; // JVAL: Mars fog sectors
        glBegin(GL_TRIANGLE_STRIP);
          glTexCoord2f(wall.ul, wall.vt);
          glVertex3f(seg.x1, wall.ytop, seg.z1);
          glTexCoord2f(wall.ul, wall.vb);
          glVertex3f(seg.x1, wall.ybottom, seg.z1);
          glTexCoord2f(wall.ur, wall.vt);
          glVertex3f(seg.x2, wall.ytop, seg.z2);
          glTexCoord2f(wall.ur, wall.vb);
          glVertex3f(seg.x2, wall.ybottom, seg.z2);
        glEnd;
        WALL_WHITEFOG_STOP; // JVAL: Mars fog sectors
      end
      else
      begin
        theight := wall.gltexture.buffer_height / MAP_COEFF;
        vm := wall.vt + (wall.ytop - wall.ymid) / theight;

        WALL_WHITEFOG_1; // JVAL: Mars fog sectors

        glBegin(GL_TRIANGLE_STRIP);
          glTexCoord2f(wall.ul, wall.vt);
          glVertex3f(seg.x1, wall.ytop, seg.z1);
          glTexCoord2f(wall.ul, vm);
          glVertex3f(seg.x1, wall.ymid, seg.z1);
          glTexCoord2f(wall.ur, wall.vt);
          glVertex3f(seg.x2, wall.ytop, seg.z2);
          glTexCoord2f(wall.ur, vm);
          glVertex3f(seg.x2, wall.ymid, seg.z2);
        glEnd;

        WALL_WHITEFOG_STOP; // JVAL: Mars fog sectors

        WALL_WHITEFOG_2; // JVAL: Mars fog sectors

        gld_StaticLightAlpha(wall.light2, wall.alpha);

        glBegin(GL_TRIANGLE_STRIP);
          glTexCoord2f(wall.ul, vm);
          glVertex3f(seg.x1, wall.ymid, seg.z1);
          glTexCoord2f(wall.ul, wall.vb);
          glVertex3f(seg.x1, wall.ybottom, seg.z1);
          glTexCoord2f(wall.ur, vm);
          glVertex3f(seg.x2, wall.ymid, seg.z2);
          glTexCoord2f(wall.ur, wall.vb);
          glVertex3f(seg.x2, wall.ybottom, seg.z2);
        glEnd;

        WALL_WHITEFOG_STOP; // JVAL: Mars fog sectors
      end;
    end;

    if wall.blend then
      glDepthMask(true)
    else
      glEnable(GL_BLEND)
  end;
end;

procedure gld_AddFlat_Extra(sectornum: integer; pic, zheight: integer;
  isfloor: boolean; ripple: boolean; angle: angle_t; anglex, angley: fixed_t);
var
  {$IFDEF DOOM_OR_STRIFE}
  tempsec: sector_t; // needed for R_FakeFlat
  {$ENDIF}
  sector: Psector_t; // the sector we want to draw
  flat: GLFlat;
begin
  if sectornum < 0 then
    exit;

  if isfloor then
  begin
    if fsectorrenderedflatex = nil then
      exit;

    if fsectorrenderedflatex[sectornum] = rendermarker then
      exit;

    fsectorrenderedflatex[sectornum] := rendermarker;
  end
  else
  begin
    if csectorrenderedflatex = nil then
      exit;

    if csectorrenderedflatex[sectornum] = rendermarker then
      exit;

    csectorrenderedflatex[sectornum] := rendermarker;
  end;

  flat.sectornum := sectornum;
  sector := @sectors[sectornum]; // get the sector
  {$IFDEF DOOM_OR_STRIFE}
  sector := R_FakeFlat(sector, @tempsec, nil, nil, false); // for boom effects
  {$ENDIF}
  flat.ceiling := true;

  // get the texture. flattranslation is maintained by doom and
  // contains the number of the current animation frame
  flat.gltexture := gld_RegisterFlat(R_GetLumpForFlat(pic), true, pic);
  if flat.gltexture = nil then
    exit;
  // get the lightlevel
  flat.light := gld_CalcLightLevel(sector.lightlevel + (extralight shl 5));
  // calculate texture offsets
  {$IFDEF DOOM_OR_STRIFE}
  flat.hasoffset := (sector.ceiling_xoffs <> 0) or (sector.ceiling_yoffs <> 0);
  if flat.hasoffset then
  begin
    flat.uoffs := sector.ceiling_xoffs / FLATUVSCALE;
    flat.voffs := sector.ceiling_yoffs / FLATUVSCALE;
  end
  else
  begin
    flat.uoffs := 0.0;
    flat.voffs := 0.0;
  end;
  {$ENDIF}
  {$IFDEF HEXEN}
  flat.hasoffset := false;
  flat.uoffs := 0.0;
  flat.voffs := 0.0;
  {$ENDIF}
  flat.ripple := ripple;

  if angle <> 0 then
  begin
    flat.angle := (angle / ANGLE_MAX) * 360.0;
    flat.anglex := anglex / FLATUVSCALE;
    flat.angley := angley / FLATUVSCALE;
    flat.hasangle := True;
  end
  else
  begin
    flat.angle := 0.0;
    flat.anglex := 0.0;
    flat.angley := 0.0;
    flat.hasangle := False;
  end;

  // get height from plane
  flat.z := zheight / MAP_SCALE;

  flat.whitefog := sector.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors

  if gld_drawinfo.num_flats >= gld_drawinfo.max_flats then
  begin
    gld_drawinfo.max_flats := gld_drawinfo.max_flats + 128;
    gld_drawinfo.flats := Z_Realloc(gld_drawinfo.flats, gld_drawinfo.max_flats * SizeOf(GLFlat), PU_LEVEL, nil);
  end;
  gld_AddDrawItem(GLDIT_FLAT, gld_drawinfo.num_flats);
  gld_drawinfo.flats[gld_drawinfo.num_flats] := flat;
  inc(gld_drawinfo.num_flats);
end;

// For mid textures (3d Floors)
procedure gld_AddFlat_3dFloor(sectornum: integer; pic, zheight: integer;
  ripple: boolean; light: integer; angle: angle_t; anglex, angley: fixed_t;
  whitefog: boolean);
var
  {$IFDEF DOOM_OR_STRIFE}
  tempsec: sector_t; // needed for R_FakeFlat
  sector: Psector_t; // the sector we want to draw
  {$ENDIF}
  flat: GLFlat;
begin
  if sectornum < 0 then
    exit;

  if sectorrenderedflatex2 = nil then
    exit;

  if sectorrenderedflatex2[sectornum] = rendermarker then
    exit;

  sectorrenderedflatex2[sectornum] := rendermarker;

  flat.sectornum := sectornum;
  {$IFDEF DOOM_OR_STRIFE}
  sector := @sectors[sectornum]; // get the sector
  sector := R_FakeFlat(sector, @tempsec, nil, nil, false); // for boom effects
  {$ENDIF}
  flat.ceiling := true;

  // get the texture. flattranslation is maintained by doom and
  // contains the number of the current animation frame
  flat.gltexture := gld_RegisterFlat(R_GetLumpForFlat(pic), true, pic);
  if flat.gltexture = nil then
    exit;
  // get the lightlevel
  flat.light := gld_CalcLightLevel(light + (extralight shl 5));
  // calculate texture offsets
  {$IFDEF DOOM_OR_STRIFE}
  flat.hasoffset := (sector.ceiling_xoffs <> 0) or (sector.ceiling_yoffs <> 0);
  if flat.hasoffset then
  begin
    flat.uoffs := sector.ceiling_xoffs / FLATUVSCALE;
    flat.voffs := sector.ceiling_yoffs / FLATUVSCALE;
  end
  else
  begin
    flat.uoffs := 0.0;
    flat.voffs := 0.0;
  end;
  {$ENDIF}
  {$IFDEF HEXEN}
  flat.hasoffset := false;
  flat.uoffs := 0.0;
  flat.voffs := 0.0;
  {$ENDIF}
  flat.ripple := ripple;

  if angle <> 0 then
  begin
    flat.angle := (angle / ANGLE_MAX) * 360.0;
    flat.anglex := anglex / FLATUVSCALE;
    flat.angley := angley / FLATUVSCALE;
    flat.hasangle := True;
  end
  else
  begin
    flat.angle := 0.0;
    flat.anglex := 0.0;
    flat.angley := 0.0;
    flat.hasangle := False;
  end;

  // get height from plane
  flat.z := zheight / MAP_SCALE;

  flat.whitefog := whitefog; // JVAL: Mars fog sectors

  if gld_drawinfo.num_flats >= gld_drawinfo.max_flats then
  begin
    gld_drawinfo.max_flats := gld_drawinfo.max_flats + 128;
    gld_drawinfo.flats := Z_Realloc(gld_drawinfo.flats, gld_drawinfo.max_flats * SizeOf(GLFlat), PU_LEVEL, nil);
  end;
  gld_AddDrawItem(GLDIT_FLAT, gld_drawinfo.num_flats);
  gld_drawinfo.flats[gld_drawinfo.num_flats] := flat;
  inc(gld_drawinfo.num_flats);
end;

// JVAL: 3d floors
procedure gld_AddMidWall(seg: Pseg_t; const ssec, msec: Psector_t);
var
  wall: GLWall;
  temptex: PGLTexture;
  rellight: integer;
  texid: integer;
  line: Pline_t;
  other: Psector_t;
begin
  wall.glseg := @gl_segs[seg.iSegID];

  if gl_fakecontrast then
  begin
    if seg.linedef.dx = 0 then
      rellight := 16 // 8
    else if seg.linedef.dy = 0 then
      rellight := -16 // -8
    else
      rellight := 0;
  end
  else
    rellight := 0;

  wall.light := gld_CalcLightLevel(ssec.lightlevel + rellight + (extralight shl 5));
  wall.whitefog := ssec.renderflags and SRF_FOG <> 0;

  if ssec = seg.frontsector then
    other := seg.backsector
  else
    other := nil;
  if other = nil then
  begin
    wall.light2 := wall.light;
    wall.whitefog2 := wall.whitefog;
    wall.doublelight := False;
  end
  else if other.midsec < 0 then
  begin
    wall.light2 := wall.light;
    wall.whitefog2 := wall.whitefog;
    wall.doublelight := False;
  end
  else
  begin
    other := @sectors[other.midsec];
    wall.light2 := gld_CalcLightLevel(other.lightlevel + rellight + (extralight shl 5));
    wall.whitefog2 := other.renderflags and SRF_FOG <> 0;
    wall.doublelight := true;
    wall.ymid := (other.floorheight / MAP_SCALE + other.ceilingheight / MAP_SCALE) / 2;
  end;


  {$IFDEF DOOM}
  if seg.linedef.renderflags and LRF_TRANSPARENT <> 0 then
    wall.alpha := 0.5
  else
  {$ENDIF}
    wall.alpha := 1.0;

  wall.gltexture := nil;

  wall.glseg.x1 := -seg.v1.x / MAP_SCALE;
  wall.glseg.z1 :=  seg.v1.y / MAP_SCALE;
  wall.glseg.x2 := -seg.v2.x / MAP_SCALE;
  wall.glseg.z2 :=  seg.v2.y / MAP_SCALE;
  wall.ytop := msec.floorheight / MAP_SCALE;
  wall.ybottom := msec.ceilingheight / MAP_SCALE;

  line := @lines[ssec.midline];

  if line.sidenum[0] > -1 then
    texid := sides[line.sidenum[0]].midtexture
  else if line.sidenum[1] > -1 then
    texid := sides[line.sidenum[1]].midtexture
  else
    exit;

  temptex := gld_RegisterTexture(texturetranslation[texid], true);
  if temptex <> nil then
  begin
    wall.gltexture := temptex;
    CALC_TEX_VALUES_MIDDLETS(
      @wall, seg, false,
      seg.length, (msec.ceilingheight - msec.floorheight) / FRACUNIT
    );
    ADDWALL(@wall);
  end;
end;

procedure gld_AddWall(seg: Pseg_t{$IFDEF HEXEN}; const ispolyobj: boolean; const ssec: Psector_t{$ENDIF});
var
  wall: GLWall;
  temptex: PGLTexture;
  frontsector: Psector_t;
  backsector: Psector_t;
  lineheight: float;
  rellight: integer;
  floor_height, ceiling_height: integer;
  floormax, ceilingmin, linelen: integer;
  mip: float;
  other: Psector_t;
{$IFDEF DOOM_OR_STRIFE}
  ftempsec: sector_t; // needed for R_FakeFlat
  btempsec: sector_t; // needed for R_FakeFlat
{$ENDIF}
label
  bottomtexture;
begin
  // mark the segment as visible for auto map
  seg.linedef.flags := seg.linedef.flags or ML_MAPPED;

  if segrendered = nil then
    exit;

  if segrendered[seg.iSegID] = rendermarker then
    exit;

  segrendered[seg.iSegID] := rendermarker;

  if seg.frontsector = nil then
    exit;

  {$IFDEF DOOM_OR_STRIFE}
  frontsector := R_FakeFlat(seg.frontsector, @ftempsec, nil, nil, false); // for boom effects
  {$ELSE}
  frontsector := seg.frontsector;
  {$ENDIF}
  wall.glseg := @gl_segs[seg.iSegID];

  if gl_fakecontrast then
  begin
    if seg.linedef.dx = 0 then
      rellight := 16 // 8
    else if seg.linedef.dy = 0 then
      rellight := -16 // -8
    else
      rellight := 0;
  end
  else
    rellight := 0;

  wall.light := gld_CalcLightLevel(frontsector.lightlevel + rellight + (extralight shl 5));
  wall.whitefog := frontsector.renderflags and SRF_FOG <> 0;

// JVAL: 3d floors
  if seg.frontsector.midsec >= 0 then
  begin
    other := @sectors[seg.frontsector.midsec];
    gld_AddMidWall(seg, seg.frontsector, other);
    wall.light2 := gld_CalcLightLevel(other.lightlevel + rellight + (extralight shl 5));
    wall.whitefog2 := other.renderflags and SRF_FOG <> 0;
    wall.doublelight := true;
    wall.ymid := (other.floorheight / MAP_SCALE + other.ceilingheight / MAP_SCALE) / 2;
  end
  else
  begin
    wall.light2 := wall.light;
    wall.whitefog2 := wall.whitefog;
    wall.doublelight := False;
  end;

  if seg.backsector <> nil then
    if seg.backsector.midsec >= 0 then
      gld_AddMidWall(seg, seg.backsector, @sectors[seg.backsector.midsec]);

  {$IFNDEF HEXEN}
  if seg.linedef.renderflags and LRF_TRANSPARENT <> 0 then
    wall.alpha := 0.5
  else
  {$ENDIF}
  {$IFDEF STRIFE}
    if (seg.linedef.flags and ML_TRANSPARENT1 <> 0) or (seg.linedef.flags and ML_TRANSPARENT2 <> 0) then
      wall.alpha := 0.75
    else
  {$ENDIF}
    wall.alpha := 1.0;

  wall.gltexture := nil;

  {$IFDEF HEXEN}
  if ispolyobj then
  begin
    wall.glseg.x1 := -seg.v1.x / MAP_SCALE;
    wall.glseg.z1 :=  seg.v1.y / MAP_SCALE;
    wall.glseg.x2 := -seg.v2.x / MAP_SCALE;
    wall.glseg.z2 :=  seg.v2.y / MAP_SCALE;
    wall.ytop := ssec.ceilingheight / MAP_SCALE;
    wall.ybottom := ssec.floorheight / MAP_SCALE;
    temptex := gld_RegisterTexture(texturetranslation[seg.sidedef.midtexture], true);
    if temptex <> nil then
    begin
      wall.gltexture := temptex;
      CALC_TEX_VALUES_MIDDLE1S(
        @wall, seg, false,
        seg.length, (ssec.ceilingheight - ssec.floorheight) / FRACUNIT
      );
      ADDWALL(@wall);
    end;
    exit;
  end;
  {$ENDIF}

  if seg.backsector = nil then // onesided
  begin
    if frontsector.ceilingpic = skyflatnum then
    begin
      wall.ytop := 255.0;
      wall.ybottom := frontsector.ceilingheight / MAP_SCALE;
      ADDSKYTEXTURE(@wall);
      ADDWALL(@wall);
    end;
    if frontsector.floorpic = skyflatnum then
    begin
      wall.ytop := frontsector.floorheight / MAP_SCALE;
      wall.ybottom := -255.0;
      ADDSKYTEXTURE(@wall);
      ADDWALL(@wall);
    end;
    temptex := gld_RegisterTexture(texturetranslation[seg.sidedef.midtexture], true);
    if (temptex <> nil) and (frontsector.ceilingheight > frontsector.floorheight) then
    begin
      wall.gltexture := temptex;
      CALC_Y_VALUES(@wall, lineheight, frontsector.floorheight, frontsector.ceilingheight);
      CALC_TEX_VALUES_MIDDLE1S(
        @wall, seg, seg.linedef.flags and ML_DONTPEGBOTTOM <> 0,
        seg.length, lineheight
      );
      ADDWALL(@wall);
    end;
  end
  // JVAL: This corrects some problems with MAP18 from Doom2 and other similar maps
  else if seg.backsector.linecount = 1 then
  begin
    if frontsector.ceilingpic = skyflatnum then
    begin
      wall.ytop := 255.0;
      wall.ybottom := frontsector.ceilingheight / MAP_SCALE;
      ADDSKYTEXTURE(@wall);
      ADDWALL(@wall);
    end;
    if frontsector.floorpic = skyflatnum then
    begin
      wall.ytop := frontsector.floorheight / MAP_SCALE;
      wall.ybottom := -255.0;
      ADDSKYTEXTURE(@wall);
      ADDWALL(@wall);
    end;
    temptex := gld_RegisterTexture(texturetranslation[seg.sidedef.toptexture], true);
    if temptex <> nil then
    begin
      wall.gltexture := temptex;
      CALC_Y_VALUES(@wall, lineheight, frontsector.floorheight, frontsector.ceilingheight);
      CALC_TEX_VALUES_MIDDLE1S(
        @wall, seg, seg.linedef.flags and ML_DONTPEGBOTTOM <> 0,
        seg.length, lineheight
      );
      ADDWALL(@wall);
    end;
  end
  else // twosided
  begin
    {$IFDEF DOOM_OR_STRIFE}
    backsector := R_FakeFlat(seg.backsector, @btempsec, nil, nil, true); // for boom effects
    {$ELSE}
    backsector := seg.backsector;
    {$ENDIF}
    // toptexture
    ceiling_height := frontsector.ceilingheight;
    floor_height := backsector.ceilingheight;
    if frontsector.ceilingpic = skyflatnum then
    begin
      wall.ytop := 255.0;
      if  // e6y
          // Fix for HOM in the starting area on Memento Mori map29 and on map30.
          // old code: (backsector.ceilingheight==backsector.floorheight) &&
          ((backsector.ceilingheight = backsector.floorheight) or (backsector.ceilingheight <= frontsector.floorheight)) and
           (backsector.ceilingpic = skyflatnum) then
      begin
        if seg.sidedef.rowoffset > 0 then
          wall.ybottom := (backsector.floorheight + seg.sidedef.rowoffset) / MAP_SCALE
        else
          wall.ybottom := backsector.floorheight / MAP_SCALE;
        ADDSKYTEXTURE(@wall);
        ADDWALL(@wall);
      end
      else
      begin
        if texturetranslation[seg.sidedef.toptexture] <> NO_TEXTURE then
        begin
          // e6y
          // It corrects some problem with sky, but I do not remember which one
          // old code: wall.ybottom= frontsector.ceilingheight/MAP_SCALE;
          if frontsector.ceilingheight > backsector.ceilingheight then
            wall.ybottom := frontsector.ceilingheight / MAP_SCALE
          else
            wall.ybottom := backsector.ceilingheight / MAP_SCALE;

          ADDSKYTEXTURE(@wall);
          ADDWALL(@wall);
        end
        else if (backsector.ceilingheight <= frontsector.floorheight) or
                (backsector.ceilingpic <> skyflatnum) then
        begin
          wall.ybottom := backsector.ceilingheight / MAP_SCALE;
          ADDSKYTEXTURE(@wall);
          ADDWALL(@wall);
        end;
      end;
    end;

    if (floor_height < ceiling_height) or
       (backsector.renderflags and SRF_SLOPED <> 0) or // JVAL: Slopes
       (frontsector.renderflags and SRF_SLOPED <> 0) then
    begin
      if not ((frontsector.ceilingpic = skyflatnum) and (backsector.ceilingpic = skyflatnum)) then
      begin
        temptex := gld_RegisterTexture(texturetranslation[seg.sidedef.toptexture], true);
        if temptex <> nil then
        begin
          wall.gltexture := temptex;
          CALC_Y_VALUES2(@wall, lineheight, floor_height, ceiling_height);
          CALC_TEX_VALUES_TOP(
            @wall, seg, (seg.linedef.flags and ML_DONTPEGTOP) = 0,
            seg.length, lineheight
          );
          ADDWALL(@wall);
        end
        else if (backsector <> nil) and (seg.linedef.renderflags and LRF_ISOLATED = 0) and
                (frontsector.ceilingpic <> skyflatnum) and (backsector.ceilingpic <> skyflatnum) then
        begin
          wall.ytop := ceiling_height / MAP_SCALE + SMALLDELTA;
          wall.ybottom := floor_height / MAP_SCALE - SMALLDELTA;
          temptex := gld_RegisterFlat(R_GetLumpForFlat(seg.backsector.ceilingpic), true, seg.backsector.ceilingpic);
          if temptex <> nil then
          begin
            wall.flag := GLDWF_TOPFLUD;
            wall.gltexture := temptex;
            ADDWALL(@wall);
          end;
//          gld_AddFlat_Extra(seg.frontsector.iSectorID, seg.backsector.ceilingpic, seg.frontsector.floorheight, true, false); // here SOS SOS JVAL 20200105
        end
      end;
    end;

    // midtexture
    temptex := gld_RegisterTexture(texturetranslation[seg.sidedef.midtexture], true);
    if temptex <> nil then
    begin
      wall.gltexture := temptex;
      if seg.linedef.flags and ML_DONTPEGBOTTOM > 0 then
      begin
        if seg.backsector.ceilingheight <= seg.frontsector.floorheight then
          goto bottomtexture;
        floor_height := gl_i_max(seg.frontsector.floorheight, seg.backsector.floorheight) + seg.sidedef.rowoffset;
        ceiling_height := floor_height + (temptex.realtexheight * FRACUNIT);
      end
      else
      begin
        if seg.backsector.ceilingheight <= seg.frontsector.floorheight then
          goto bottomtexture;
        ceiling_height := gl_i_min(seg.frontsector.ceilingheight, seg.backsector.ceilingheight) + seg.sidedef.rowoffset;
        floor_height := ceiling_height - (temptex.realtexheight * FRACUNIT);
      end;

      mip := temptex.realtexheight / temptex.buffer_height;
      if seg.sidedef.bottomtexture <> 0 then
        floormax := gl_i_max(seg.frontsector.floorheight, seg.backsector.floorheight)
      else
        floormax := floor_height;
      if seg.sidedef.toptexture <> 0 then
        ceilingmin := gl_i_min(seg.frontsector.ceilingheight, seg.backsector.ceilingheight)
      else
        ceilingmin := ceiling_height;
      linelen := abs(ceiling_height - floor_height);
      wall.ytop := gl_i_min(ceilingmin, ceiling_height) / MAP_SCALE;
      wall.ybottom := gl_i_max(floormax, floor_height) / MAP_SCALE;
      wall.flag := GLDWF_M2S;
      wall.ul := OU(temptex, seg);
      wall.ur := wall.ul + (seg.length / temptex.buffer_width);
      if floormax <= floor_height then
        wall.vb := mip
      else
        wall.vb := mip * (ceiling_height - floormax) / linelen;
      if ceilingmin >= ceiling_height then
        wall.vt := 0.0
      else
        wall.vt := mip * (ceiling_height - ceilingmin) / linelen;
{      if (seg.linedef.tranlump >= 0) and general_translucency)
        wall.alpha= tran_filter_pct/100.0;}
      ADDWALL(@wall);
//      wall.alpha := 1.0;
    end;

bottomtexture:
    ceiling_height := backsector.floorheight;
    floor_height := frontsector.floorheight;
    if frontsector.floorpic = skyflatnum then
    begin
      wall.ybottom := -255.0;
      if (
          (backsector.ceilingheight = backsector.floorheight) and
          (backsector.floorpic = skyflatnum)
         ) then
      begin
        wall.ytop := backsector.floorheight / MAP_SCALE;
        ADDSKYTEXTURE(@wall);
        ADDWALL(@wall);
      end
      else
      begin
        if texturetranslation[seg.sidedef.bottomtexture] <> NO_TEXTURE then
        begin
          wall.ytop := frontsector.floorheight / MAP_SCALE;
          ADDSKYTEXTURE(@wall);
          ADDWALL(@wall);
        end
        else
          if (backsector.floorheight >= frontsector.ceilingheight) or
             (backsector.floorpic <> skyflatnum) then
          begin
            wall.ytop := backsector.floorheight / MAP_SCALE;
            ADDSKYTEXTURE(@wall);
            ADDWALL(@wall);
          end;
      end;
    end;
    if (floor_height < ceiling_height) or
       (backsector.renderflags and SRF_SLOPED <> 0) or
       (frontsector.renderflags and SRF_SLOPED <> 0) then // JVAL: Slopes
    begin
      if (frontsector.floorpic <> skyflatnum) and // JVAL 21/5/2011
         (backsector.floorheight > frontsector.floorheight) and
         (texturetranslation[seg.sidedef.bottomtexture] = NO_TEXTURE) then
      begin
        wall.ytop := ceiling_height / MAP_SCALE + SMALLDELTA;
        wall.ybottom := floor_height / MAP_SCALE - SMALLDELTA;
        if wall.ytop <= zCamera then
        begin
          temptex := gld_RegisterFlat(R_GetLumpForFlat(seg.backsector.floorpic), true, seg.backsector.floorpic);
          if temptex <> nil then
          begin
            wall.flag := GLDWF_BOTFLUD;
            wall.gltexture := temptex;
            ADDWALL(@wall);
          end;
        end;
        //gld_AddFlat_Extra(seg.frontsector.iSectorID, seg.backsector.floorpic, seg.backsector.floorheight, true, false); // here
      end
      else
      begin
        temptex := gld_RegisterTexture(texturetranslation[seg.sidedef.bottomtexture], true);
        if temptex <> nil then
        begin
          wall.gltexture := temptex;
          CALC_Y_VALUES2(@wall, lineheight, floor_height, ceiling_height);
          CALC_TEX_VALUES_BOTTOM(
            @wall, seg, (seg.linedef.flags and ML_DONTPEGBOTTOM) <> 0,
            seg.length, lineheight,
            floor_height - frontsector.ceilingheight
          );
          ADDWALL(@wall);
        end
        else if (backsector <> nil) and (seg.linedef.renderflags and LRF_ISOLATED = 0) and
                (frontsector.ceilingpic <> skyflatnum) and (backsector.ceilingpic <> skyflatnum) then
        begin
          gld_AddFlat_Extra(
            seg.frontsector.iSectorID, seg.backsector.floorpic, seg.frontsector.floorheight,
            False, seg.frontsector.renderflags and SRF_RIPPLE_CEILING <> 0,
            seg.backsector.floorangle, seg.backsector.flooranglex, seg.backsector.floorangley);
        end;
      end;
    end;
  end;
end;

procedure gld_PreprocessSegs;
var
  i: integer;
begin
  gl_segs := Z_Malloc(numsegs * SizeOf(TGLSeg), PU_LEVEL, nil);
  for i := 0 to numsegs - 1 do
  begin
    gl_segs[i].x1 := -segs[i].v1.x / MAP_SCALE;
    gl_segs[i].z1 :=  segs[i].v1.y / MAP_SCALE;
    gl_segs[i].x2 := -segs[i].v2.x / MAP_SCALE;
    gl_segs[i].z2 :=  segs[i].v2.y / MAP_SCALE;
    gl_segs[i].frontsector := segs[i].frontsector;
    gl_segs[i].backsector := segs[i].backsector;
  end;
end;

(*****************
 *               *
 * Flats         *
 *               *
 *****************)

var
  rippletexmatrix: array[0..15] of TGLfloat;
  ripplelastfrac: Integer = -1;

procedure gld_MakeRippleMatrix;
var
  rsin, rcos: TGLfloat;
  frac: integer;
begin
  frac := leveltime and 63;
  if ripplelastfrac = frac then
    exit;

  ripplelastfrac := frac;

  rsin := 0.01 * Sin(frac / 64 * 2 * __glPi);
  rcos := 0.01 * Cos(frac / 64 * 2 * __glPi);

  rippletexmatrix[0 + 4 * 0] := 1;
  rippletexmatrix[0 + 4 * 1] := rsin;
  rippletexmatrix[0 + 4 * 2] := rsin;
  rippletexmatrix[0 + 4 * 3] := 0;

  rippletexmatrix[1 + 4 * 0] := rcos;
  rippletexmatrix[1 + 4 * 1] := 1;
  rippletexmatrix[1 + 4 * 2] := rcos;
  rippletexmatrix[1 + 4 * 3] := 0;

  rippletexmatrix[2 + 4 * 0] := 0;
  rippletexmatrix[2 + 4 * 1] := 0;
  rippletexmatrix[2 + 4 * 2] := 1;
  rippletexmatrix[2 + 4 * 3] := 0;

  rippletexmatrix[3 + 4 * 0] := 0;
  rippletexmatrix[3 + 4 * 1] := 0;
  rippletexmatrix[3 + 4 * 2] := 0;
  rippletexmatrix[3 + 4 * 3] := 1;
end;

procedure gld_DrawFlat(flat: PGLFlat);
var
  loopnum, i: integer; // current loop number
  currentloop: PGLLoopDef; // the current loop
  glsec: PGLSector;
  sec: Psector_t;
  fz: float;
begin
  if flat.sectornum < 0 then
    exit;

{  if flat.ceiling then
    glCullFace(GL_BACK)
  else
    glCullFace(GL_FRONT);   }

  glsec := @sectorloops[flat.sectornum];
  sec := @sectors[flat.sectornum];
  if glsec.list = 0 then
  begin
    if glsec.loopcount > 0 then
    begin
      glsec.list := glGenLists(1);

      if glsec.list > 0 then
      begin
        glNewList(glsec.list, GL_COMPILE);

        for loopnum := 0 to glsec.loopcount - 1 do
        begin
          // set the current loop
          currentloop := @glsec.loops[loopnum];
          glBegin(currentloop.mode);
          for i := currentloop.vertexindex to currentloop.vertexindex + currentloop.vertexcount - 1 do
          begin
            glTexCoord2f(gld_texcoords[i].u * flat.gltexture.texturescale, gld_texcoords[i].v * flat.gltexture.texturescale);
            glVertex3fv(@gld_vertexes[i]);
          end;
          glEnd;
        end;

        glEndList;

        if G_PlayingEngineVersion < VERSIONSLOPES then
          Z_Free(glsec.loops);  // JVAL: Slopes
      end
      else
        glsec.list := GL_BAD_LIST;
    end
    else
      glsec.list := GL_BAD_LIST;
  end;

  if gl_uselightmaps then
  begin
    glActiveTextureARB(GL_TEXTURE1_ARB);
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    glTranslatef(0, flat.z * MAP_COEFF / (LIGHTMAPSIZEY * LIGHTMAPUNIT), 0);
    glActiveTextureARB(GL_TEXTURE0_ARB);
  end;

  if flat.whitefog then gld_StartWhiteFog;  // JVAL: Mars fog sectors

  gld_BindFlat(flat.gltexture);
  gld_StaticLight(flat.light);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  fz := flat.z;
  glTranslatef(0.0, fz, 0.0);
  {$IFNDEF HERETIC}
  if flat.hasoffset then
  begin
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    glTranslatef(flat.uoffs * flat.gltexture.texturescale {$IFDEF HEXEN}* 64 / flat.gltexture.width{$ENDIF},
                 flat.voffs * flat.gltexture.texturescale {$IFDEF HEXEN}* 64 / flat.gltexture.height{$ENDIF},
                 0.0);
  end;
  {$ENDIF}

  glActiveTextureARB(GL_TEXTURE0_ARB);

  if flat.hasangle then
  begin
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    glTranslatef(
      flat.anglex * flat.gltexture.texturescale {$IFDEF HEXEN}* 64 / flat.gltexture.width{$ENDIF},
      -flat.angley * flat.gltexture.texturescale {$IFDEF HEXEN}* 64 / flat.gltexture.height{$ENDIF},
      0.0);
    glRotatef(flat.angle, 0, 0, 1);
    glTranslatef(
      -flat.anglex * flat.gltexture.texturescale {$IFDEF HEXEN}* 64 / flat.gltexture.width{$ENDIF},
      flat.angley * flat.gltexture.texturescale {$IFDEF HEXEN}* 64 / flat.gltexture.height{$ENDIF},
      0.0);
  end;

  if flat.ripple then
  begin
    gld_MakeRippleMatrix;
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    glMultMatrixf(@rippletexmatrix);
  end;

  if not flat.ceiling and (sec.renderflags and SRF_SLOPEFLOOR <> 0) then
  begin
    // go through all loops of this sector
    for loopnum := 0 to glsec.loopcount - 1 do
    begin
      currentloop := @glsec.loops[loopnum];
      glBegin(currentloop.mode);
      for i := currentloop.vertexindex to currentloop.vertexindex + currentloop.vertexcount - 1 do
      begin
        glTexCoord2f(gld_texcoords[i].u * flat.gltexture.texturescale, gld_texcoords[i].v * flat.gltexture.texturescale);
        glVertex3f(gld_vertexes[i].x, gld_FloorHeight(sec, gld_vertexes[i].x, gld_vertexes[i].z) - fz, gld_vertexes[i].z)
      end;
      glEnd;
    end;
  end
  else if flat.ceiling and (sec.renderflags and SRF_SLOPECEILING <> 0) then
  begin
    // go through all loops of this sector
    for loopnum := 0 to glsec.loopcount - 1 do
    begin
      currentloop := @glsec.loops[loopnum];
      glBegin(currentloop.mode);
      for i := currentloop.vertexindex to currentloop.vertexindex + currentloop.vertexcount - 1 do
      begin
        glTexCoord2f(gld_texcoords[i].u * flat.gltexture.texturescale, gld_texcoords[i].v * flat.gltexture.texturescale);
        glVertex3f(gld_vertexes[i].x, gld_CeilingHeight(sec, gld_vertexes[i].x, gld_vertexes[i].z) - fz, gld_vertexes[i].z)
      end;
      glEnd;
    end;
  end
  else
  begin
    // JVAL: Call the precalced list if available
    if glsec.list <> GL_BAD_LIST then
      glCallList(glsec.list)
    else
    begin
    // go through all loops of this sector
      for loopnum := 0 to glsec.loopcount - 1 do
      begin
          // set the current loop
        currentloop := @glsec.loops[loopnum];
        glDrawArrays(currentloop.mode, currentloop.vertexindex, currentloop.vertexcount);
      end;
    end;
  end;
  if flat.ripple then
  begin
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
  end;

  glActiveTextureARB(GL_TEXTURE0_ARB);
  if flat.hasangle then
  begin
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glActiveTextureARB(GL_TEXTURE0_ARB);
  end;

  {$IFNDEF HERETIC}
  if flat.hasoffset then
  begin
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
  end;
  {$ENDIF}
  if gl_uselightmaps then
  begin
    glActiveTextureARB(GL_TEXTURE1_ARB);
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glActiveTextureARB(GL_TEXTURE0_ARB);
  end;

  if flat.whitefog then gld_StartFog;  // JVAL: Mars fog sectors

  glPopMatrix;
end;

//
// gld_AddFlat
//
// This draws on flat for the sector 'num'
// The ceiling boolean indicates if the flat is a floor(false) or a ceiling(true)
//
procedure gld_AddFlat(sectornum: integer; ceiling: boolean; plane: Pvisplane_t);
var
  {$IFDEF DOOM_OR_STRIFE}
  tempsec: sector_t; // needed for R_FakeFlat
  {$ENDIF}
  sector: Psector_t; // the sector we want to draw
  flat: GLFlat;
  msec: Psector_t;  // JVAL: 3d Floors
begin
  if sectornum < 0 then
    exit;

  flat.sectornum := sectornum;
  sector := @sectors[sectornum]; // get the sector
  {$IFDEF DOOM_OR_STRIFE}
  sector := R_FakeFlat(sector, @tempsec, nil, nil, false); // for boom effects
  {$ENDIF}
  flat.ceiling := ceiling;
  if ceiling then // if it is a ceiling ...
  begin
    if sector.ceilingpic = skyflatnum then // don't draw if sky
      exit;
    // get the texture. flattranslation is maintained by doom and
    // contains the number of the current animation frame
    flat.gltexture := gld_RegisterFlat(R_GetLumpForFlat(sector.ceilingpic), true, sector.ceilingpic);
    if flat.gltexture = nil then
      exit;
    // get the lightlevel from floorlightlevel
    flat.light := gld_CalcLightLevel({$IFDEF DOOM_OR_STRIFE}sector.ceilinglightlevel{$ELSE}sector.lightlevel{$ENDIF} + (extralight shl 5));
    // calculate texture offsets
    {$IFDEF DOOM_OR_STRIFE}
    flat.hasoffset := (sector.ceiling_xoffs <> 0) or (sector.ceiling_yoffs <> 0);
    if flat.hasoffset then
    begin
      flat.uoffs := sector.ceiling_xoffs / FLATUVSCALE;
      flat.voffs := sector.ceiling_yoffs / FLATUVSCALE;
    end
    else
    begin
      flat.uoffs := 0.0;
      flat.voffs := 0.0;
    end;
    {$ENDIF}
    {$IFDEF HEXEN}
    flat.hasoffset := (plane.xoffs <> 0) or (plane.yoffs <> 0);
    if flat.hasoffset then
    begin
      flat.uoffs := plane.xoffs / FLATUVSCALE;
      flat.voffs := plane.yoffs / FLATUVSCALE;
    end
    else
    begin
      flat.uoffs := 0.0;
      flat.voffs := 0.0;
    end;
    {$ENDIF}
    flat.ripple := plane.renderflags and SRF_RIPPLE_CEILING <> 0;

    if plane.angle <> 0 then
    begin
      flat.angle := (plane.angle / ANGLE_MAX) * 360.0;
      flat.anglex := plane.anglex / FLATUVSCALE;
      flat.angley := plane.angley / FLATUVSCALE;
      flat.hasangle := True;
    end
    else
    begin
      flat.angle := 0.0;
      flat.anglex := 0.0;
      flat.angley := 0.0;
      flat.hasangle := False;
    end;
    flat.whitefog := sector.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors
  end
  else // if it is a floor ...
  begin
    if sector.floorpic = skyflatnum then // don't draw if sky
      exit;
    // get the texture. flattranslation is maintained by doom and
    // contains the number of the current animation frame
    flat.gltexture := gld_RegisterFlat(R_GetLumpForFlat(sector.floorpic), true, sector.floorpic);
    if flat.gltexture = nil then
      exit;
    // get the lightlevel from ceilinglightlevel

    if sector.midsec >= 0 then  // JVAL: 3d Floors
    begin
      msec := @sectors[sector.midsec];
      flat.light := gld_CalcLightLevel({$IFDEF DOOM_OR_STRIFE}msec.floorlightlevel{$ELSE}msec.lightlevel{$ENDIF} + (extralight shl 5));
      flat.whitefog := msec.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors
    end
    else
    begin
      flat.light := gld_CalcLightLevel({$IFDEF DOOM_OR_STRIFE}sector.floorlightlevel{$ELSE}sector.lightlevel{$ENDIF} + (extralight shl 5));
      flat.whitefog := sector.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors
    end;
    // calculate texture offsets
    {$IFDEF DOOM_OR_STRIFE}
    flat.hasoffset := (sector.floor_xoffs <> 0) or (sector.floor_yoffs <> 0);
    if flat.hasoffset then
    begin
      flat.uoffs := sector.floor_xoffs / FLATUVSCALE;
      flat.voffs := sector.floor_yoffs / FLATUVSCALE;
    end
    else
    begin
      flat.uoffs := 0.0;
      flat.voffs := 0.0;
    end;
    {$ENDIF}
    {$IFDEF HEXEN}
    flat.hasoffset := (plane.xoffs <> 0) or (plane.yoffs <> 0);
    if flat.hasoffset then
    begin
      flat.uoffs := plane.xoffs / FLATUVSCALE;
      flat.voffs := plane.yoffs / FLATUVSCALE;
    end
    else
    begin
      flat.uoffs := 0.0;
      flat.voffs := 0.0;
    end;
    {$ENDIF}
    flat.ripple := plane.renderflags and SRF_RIPPLE_FLOOR <> 0;

    if plane.angle <> 0 then
    begin
      flat.angle := (plane.angle / ANGLE_MAX) * 360.0;
      flat.anglex := plane.anglex / FLATUVSCALE;
      flat.angley := plane.angley / FLATUVSCALE;
      flat.hasangle := True;
    end
    else
    begin
      flat.angle := 0.0;
      flat.anglex := 0.0;
      flat.angley := 0.0;
      flat.hasangle := False;
    end;
  end;

  // get height from plane
  flat.z := plane.height / MAP_SCALE;

  if gld_drawinfo.num_flats >= gld_drawinfo.max_flats then
  begin
    gld_drawinfo.max_flats := gld_drawinfo.max_flats + 128;
    gld_drawinfo.flats := Z_Realloc(gld_drawinfo.flats, gld_drawinfo.max_flats * SizeOf(GLFlat), PU_LEVEL, nil);
  end;
  gld_AddDrawItem(GLDIT_FLAT, gld_drawinfo.num_flats);
  gld_drawinfo.flats[gld_drawinfo.num_flats] := flat;
  inc(gld_drawinfo.num_flats);
end;

procedure gld_AddPlane(subsectornum: integer; floor, ceiling: Pvisplane_t);
var
  subsector: Psubsector_t;
  secID: integer;
  sec: Psector_t;
  msec: Psector_t; // JVAL: 3d floors
begin
  // check if all arrays are allocated
  if sectorrendered = nil then
    exit;

  subsector := @subsectors[subsectornum];
  {$IFDEF DEBUG}
  if subsector = nil then // JVAL unused?
    exit;
  {$ENDIF}

  sec := subsector.sector;
  secID := sec.iSectorID;
  if sectorrendered[secID] <> rendermarker then // if not already rendered
  begin
    // render the floor
    if floor <> nil then
      if (floor.height < viewz) or (sectors[secID].renderflags and SRF_SLOPEFLOOR <> 0) then
        gld_AddFlat(secID, false, floor);
    // render the ceiling
    if ceiling <> nil then
      if (ceiling.height > viewz)  or (sectors[secID].renderflags and SRF_SLOPECEILING <> 0) then
        gld_AddFlat(secID, true, ceiling);

    if sectors[secID].midsec >= 0 then
    begin
      msec := @sectors[sectors[secID].midsec];
      if viewz < msec.floorheight then
        gld_AddFlat_3dFloor(
          secID, msec.floorpic, msec.floorheight, msec.renderflags and SRF_RIPPLE_FLOOR <> 0,
          msec.lightlevel, msec.floorangle, msec.flooranglex, msec.floorangley,
          msec.renderflags and SRF_FOG <> 0);
      if viewz > msec.ceilingheight then
        gld_AddFlat_3dFloor(
          secID, msec.ceilingpic, msec.ceilingheight, msec.renderflags and SRF_RIPPLE_CEILING <> 0,
          sectors[secID].lightlevel, msec.ceilingangle, msec.ceilinganglex, msec.ceilingangley,
          sectors[secID].renderflags and SRF_FOG <> 0);
    end;

    // set rendered true
    sectorrendered[secID] := rendermarker;
  end;
end;

(**********************************
 *                                *
 * Dynamic Lights / shadows       *
 *                                *
 **********************************)

procedure gld_MarkDShadow(sprite: PGLSprite);
var
  olddlitems: integer;
  dx, dy, dz: single;
  xdist, ydist, zdist: single;
  psl: Pdlsortitem_t;
begin
  xdist := camera.position[0] - sprite.x;
  ydist := camera.position[1] - sprite.y;
  zdist := camera.position[2] - sprite.z;

  if numdlitems >= realdlitems then
  begin
    olddlitems := realdlitems;
    realdlitems := numdlitems + 32;
    realloc(pointer(dlbuffer), olddlitems * SizeOf(dlsortitem_t), realdlitems * SizeOf(dlsortitem_t));
  end;

  psl := @dlbuffer[numdlitems];
  psl.l := gld_GetDynamicShadow(sprite.mo.info.radius div FRACUNIT);
  dx := xdist;
  dy := ydist;
  dz := zdist;
  psl.squaredist := dx * dx + dy * dy + dz * dz;
  psl.x := sprite.x;
  psl.y := sprite.mo.floorz / MAP_SCALE;
  psl.z := sprite.z;
  inc(numdlitems);
end;

procedure gld_MarkDLights(sprite: PGLSprite);
var
  olddlitems: integer;
  l: PGLDRenderLight;
  i: integer;
  dx, dy, dz: single;
  xdist, ydist, zdist: single;
  psl: Pdlsortitem_t;
begin
  if sprite.dlights = nil then
    exit;

  xdist := camera.position[0] - sprite.x;
  ydist := camera.position[1] - sprite.y;
  zdist := camera.position[2] - sprite.z;

  for i := 0 to sprite.dlights.Count - 1 do
    if sprite.dlights[i].num1 = sprite.mo._type then
    begin
      l := R_GetDynamicLight(sprite.dlights[i].num2);
      if numdlitems >= realdlitems then
      begin
        olddlitems := realdlitems;
        realdlitems := numdlitems + 32;
        realloc(pointer(dlbuffer), olddlitems * SizeOf(dlsortitem_t), realdlitems * SizeOf(dlsortitem_t));
      end;

      psl := @dlbuffer[numdlitems];
      psl.l := l;
//    Psubsector_t(sprite.mo.subsector).sector.floorheight
      dx := xdist - l.x;
      dy := ydist - l.y;
      dz := zdist - l.z;
      psl.squaredist := dx * dx + dy * dy + dz * dz;
      psl.x := sprite.x + l.x;
      psl.y := sprite.y + l.y;
      psl.z := sprite.z + l.z;
      inc(numdlitems);
    end;
end;

(*****************
 *               *
 * Sprites       *
 *               *
 *****************)

function gld_FindNextModelFrame(const mo: PMobj_t; const modelidx: integer): integer;
var
  i: integer;
  idx: integer;
  stnum: integer;
  st: Pstate_t;
begin
  stnum := Ord(mo.state.nextstate);
  if stnum < 1 then
  begin
    result := -1;
    exit;
  end;
  st := @states[stnum];
  if st.models = nil then
  begin
    I_DevWarning('gld_FindNextModelFrame(): Missing model information on state %s'#13#10, [statenames.Strings[stnum]]);
    result := -1;
    exit;
  end;
  if st = mo.state then
  begin
    for i := 0 to st.models.Count - 1 do
    begin
      idx := st.models.Numbers[i];
      if modelstates[idx].modelidx = modelidx then
      begin
        result := modelstates[idx].endframe;
        exit;
      end;
    end;
  end
  else
  begin
    for i := 0 to st.models.Count - 1 do
    begin
      idx := st.models.Numbers[i];
      if modelstates[idx].modelidx = modelidx then
      begin
        result := modelstates[idx].startframe;
        exit;
      end;
    end;
  end;
  result := -1;
end;

procedure gld_DrawModel(sprite: PGLSprite; const idx: integer);
var
  info: Pmodelstate_t;
  texitem: Ptexturemanagetitem_t;
  modelinf: Pmodelmanageritem_t;
  nextframe: integer;
  restoreblend: Boolean;
  restoreequation: Boolean;
begin
  info := @modelstates[idx];

  restoreblend := false;
  restoreequation := false;

  if sprite.flags and GLS_SHADOW <> 0 then
  begin
    glBlendFunc(GL_DST_COLOR, GL_ONE_MINUS_SRC_ALPHA);
    glAlphaFunc(GL_GEQUAL, 0.1);
    glColor4f(0.2, 0.2, 0.2, 0.33);
    restoreblend := true;
  end
  else
  begin
    if sprite.flags and GLS_TRANSPARENT <> 0 then
    begin
      gld_StaticLightAlpha(sprite.light, sprite.alpha);
      glAlphaFunc(GL_GEQUAL, 0.01);
      restoreblend := true;
    end
    else if sprite.flags and GLS_ADDITIVE <> 0 then
    begin
      gld_StaticLightAlpha(sprite.light, sprite.alpha);
      glAlphaFunc(GL_GEQUAL, 0.01);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      restoreblend := true;
    end
    else if sprite.flags and GLS_SUBTRACTIVE <> 0 then
    begin
      gld_StaticLightAlpha(sprite.light, sprite.alpha);
      glAlphaFunc(GL_GEQUAL, 0.01);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      glBlendEquation(GL_FUNC_REVERSE_SUBTRACT);
      restoreblend := true;
      restoreequation := true;
    end
    else if info.transparency < 0.9999 then
    begin
      gld_StaticLightAlpha(sprite.light, info.transparency);
      glAlphaFunc(GL_GEQUAL, 0.01);
      restoreblend := true;
    end
    else
      gld_StaticLight(sprite.light);
  end;

  last_gltexture := nil;
  if info.texture >= 0 then
  begin
    texitem := @modeltexturemanager.items[info.texture];
    if texitem.tex = 0 then
    begin
      texitem.tex := gld_LoadExternalTexture(texitem.name, true, GL_REPEAT);
      if texitem.tex = 0 then
        I_DevError('gld_DrawModel(): Can not load texture %s'#13#10, [texitem.name]);
    end;

    glBindTexture(GL_TEXTURE_2D, texitem.tex);
  end
  else
    glBindTexture(GL_TEXTURE_2D, 0);

  modelinf := @modelmanager.items[info.modelidx];
  if modelinf.model = nil then
  begin
    modelinf.model :=
      TModel.Create(modelinf.name, modelinf.proc,
        modelinf.xoffset, modelinf.yoffset, modelinf.zoffset,
        modelinf.xscale, modelinf.yscale, modelinf.zscale,
        modelinf.framemerge
      );
    if modelinf.model = nil then
    begin
      I_Warning('gld_DrawModel(): Can not load model %s'#13#10, [modelinf.name]);
      exit;
    end;
    modelinf.model.DrawSimple(info.startframe);
    if restoreblend then
      glAlphaFunc(GL_GEQUAL, 0.01);
    if restoreequation then
      glBlendEquation(GL_FUNC_ADD);
    exit;
  end;
  {$IFDEF DEBUG}
  printf('**drawing model %d'#13#10, [idx]);
  {$ENDIF}

  if modelinf.model.modeltype in [mt_ddmodel, mt_dmx] then
  begin
    if isgamesuspended then
    begin
      modelinf.model.DrawSimple(modelinf.model.lastdrawframe);
    end
    else
    begin
      nextframe := gld_FindNextModelFrame(sprite.mo, info.modelidx);
      modelinf.model.Draw(info.startframe, nextframe, 1.0 - (sprite.mo.tics - ticfrac / FRACUNIT) / sprite.mo.state.tics);
    end;
  end
  else if gl_smoothmodelmovement and not isgamesuspended and ((sprite.aproxdist < MODELINTERPOLATERANGE) or (modelinf.model.modeltype = mt_dll)) then
  begin
    nextframe := gld_FindNextModelFrame(sprite.mo, info.modelidx);
    modelinf.model.Draw(info.startframe, nextframe, 1.0 - (sprite.mo.tics - ticfrac / FRACUNIT) / sprite.mo.state.tics);
  end
  else
    modelinf.model.DrawSimple(info.startframe);

  if restoreblend then
    glAlphaFunc(GL_GEQUAL, 0.01);
  if restoreequation then
    glBlendEquation(GL_FUNC_ADD);
end;

procedure gld_DrawModels(sprite: PGLSprite);
var
  i: integer;
begin
  if gl_uselightmaps then
  begin
    glActiveTextureARB(GL_TEXTURE1_ARB);
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    glTranslatef(sprite.x * MAP_COEFF / (LIGHTMAPSIZEX * LIGHTMAPUNIT),
                 sprite.y * MAP_COEFF / (LIGHTMAPSIZEY * LIGHTMAPUNIT),
                 sprite.z * MAP_COEFF / (LIGHTMAPSIZEZ * LIGHTMAPUNIT));
    glActiveTextureARB(GL_TEXTURE0_ARB);
  end;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glTranslatef(sprite.x, sprite.y, sprite.z);
  glRotatef(sprite.mo.angle / (ANGLE_MAX / 360.0) - 90.0, 0.0, 1.0, 0.0);

  // JVAL
  // Draw light effects (only if not invulnerability)
  if uselightboost then
    if not lightdeflumppresent then
      if players[displayplayer].fixedcolormap <> 32 then
      // JVAL: Use old color effects only if LIGHTDEF lump not found
        if sprite.flags and GLS_LIGHT <> 0 then
        begin
          if sprite.flags and GLS_WHITELIGHT <> 0 then
            gld_SetUplight(1.0, 1.0, 1.0)
          else if sprite.flags and GLS_REDLIGHT <> 0 then
            gld_SetUplight(1.0, 0.0, 0.0)
          else if sprite.flags and GLS_GREENLIGHT <> 0 then
            gld_SetUplight(0.0, 1.0, 0.0)
          else if sprite.flags and GLS_BLUELIGHT <> 0 then
            gld_SetUplight(0.0, 0.0, 1.0)
          else if sprite.flags and GLS_YELLOWLIGHT <> 0 then
            gld_SetUplight(1.0, 1.0, 0.0);
          glBegin(GL_TRIANGLE_STRIP);
            glTexCoord2f(0.0, 0.0);
            glVertex3f(2.0 * sprite.x1, 2.0 * sprite.y1, 0.01);
            glTexCoord2f(1.0, 0.0);
            glVertex3f(2.0 * sprite.x2, 2.0 * sprite.y1, 0.01);
            glTexCoord2f(0.0, 1.0);
            glVertex3f(2.0 * sprite.x1, 2.0 * sprite.y2, 0.01);
            glTexCoord2f(1.0, 1.0);
            glVertex3f(2.0 * sprite.x2, 2.0 * sprite.y2, 0.01);
          glEnd;
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
          glAlphaFunc(GL_GEQUAL, 0.5);
        end;

  for i := 0 to sprite.models.Count - 1 do
    gld_DrawModel(sprite, sprite.models.Numbers[i]);

  glPopMatrix;

  if gl_uselightmaps then
  begin
    glActiveTextureARB(GL_TEXTURE1_ARB);
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glActiveTextureARB(GL_TEXTURE0_ARB);
  end;

  if sprite.flags and (GLS_SHADOW or GLS_TRANSPARENT or GLS_ADDITIVE or GLS_SUBTRACTIVE) <> 0 then
  begin
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glAlphaFunc(GL_GEQUAL, 0.5);
    if sprite.flags and GLS_SUBTRACTIVE <> 0 then
      glBlendEquation(GL_FUNC_ADD);
  end;
  glColor3f(1.0, 1.0, 1.0);

end;

procedure gld_DrawVoxel(sprite: PGLSprite; const idx: integer);
var
  info: Pvoxelstate_t;
  voxelinf: Pvoxelmanageritem_t;
  restoreblend: Boolean;
  restoreequation: Boolean;
  anglediff, spinang: angle_t;
begin
  info := @voxelstates[idx];

  restoreblend := false;
  restoreequation := false;

  if sprite.flags and GLS_SHADOW <> 0 then
  begin
    glBlendFunc(GL_DST_COLOR, GL_ONE_MINUS_SRC_ALPHA);
    glAlphaFunc(GL_GEQUAL, 0.1);
    glColor4f(0.2, 0.2, 0.2, 0.33);
    restoreblend := true;
  end
  else
  begin
    if sprite.flags and GLS_TRANSPARENT <> 0 then
    begin
      gld_StaticLightAlpha(sprite.light, sprite.alpha);
      glAlphaFunc(GL_GEQUAL, 0.01);
      restoreblend := true;
    end
    else if sprite.flags and GLS_ADDITIVE <> 0 then
    begin
      gld_StaticLightAlpha(sprite.light, sprite.alpha);
      glAlphaFunc(GL_GEQUAL, 0.01);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      restoreblend := true;
    end
    else if sprite.flags and GLS_SUBTRACTIVE <> 0 then
    begin
      gld_StaticLightAlpha(sprite.light, sprite.alpha);
      glAlphaFunc(GL_GEQUAL, 0.01);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      glBlendEquation(GL_FUNC_REVERSE_SUBTRACT);
      restoreblend := true;
      restoreequation := true;
    end

{    else if info.transparency < 0.9999 then
    begin
      gld_StaticLightAlpha(sprite.light, info.transparency);
      glAlphaFunc(GL_GEQUAL, 0.01);
      restoreblend := true;
    end}
    else
      gld_StaticLight(sprite.light);
  end;

  last_gltexture := nil;
//  glBindTexture(GL_TEXTURE_2D, 0);

  voxelinf := @voxelmanager.items[info.voxelidx];
  if voxelinf.voxel = nil then
  begin
    voxelinf.voxel := TVoxelModel.Create(voxelinf.name, voxelinf.offset, voxelinf.scale);
    if voxelinf.voxel = nil then
    begin
      I_Warning('gld_DrawVoxel(): Can not load voxel %s'#13#10, [voxelinf.name]);
      if restoreblend then
        glAlphaFunc(GL_GEQUAL, 0.01);
      if restoreequation then
        glBlendEquation(GL_FUNC_ADD);
      exit;
    end;
  end;
  {$IFDEF DEBUG}
  printf('**drawing voxel %d'#13#10, [idx]);
  {$ENDIF}


  anglediff := voxelinf.angleoffset * ANG1;
  {$IFDEF HEXEN}
  if sprite.mo.flags2 and MF2_DROPPED <> 0 then
  {$ELSE}
  if sprite.mo.flags and MF_DROPPED <> 0 then
  {$ENDIF}
      spinang := voxelinf.droppedspin
    else
      spinang := voxelinf.placedspin;
  if spinang <> 0 then
  begin
    if not isgamesuspended then
      spinang := Round(((leveltime + ticfrac / FRACUNIT) / TICRATE * spinang) * (ANGLE_MAX / 360))
    else
      spinang := Round((leveltime / TICRATE * spinang) * (ANGLE_MAX / 360));
  end;
  anglediff := spinang - anglediff;

  voxelinf.voxel.Draw(info.frame, anglediff);

  if restoreblend then
    glAlphaFunc(GL_GEQUAL, 0.01);
  if restoreequation then
    glBlendEquation(GL_FUNC_ADD);
end;

procedure gld_DrawVoxels(sprite: PGLSprite);
var
  i: integer;
begin
  if gl_uselightmaps then
  begin
    glActiveTextureARB(GL_TEXTURE1_ARB);
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    glTranslatef(sprite.x * MAP_COEFF / (LIGHTMAPSIZEX * LIGHTMAPUNIT),
                 sprite.y * MAP_COEFF / (LIGHTMAPSIZEY * LIGHTMAPUNIT),
                 sprite.z * MAP_COEFF / (LIGHTMAPSIZEZ * LIGHTMAPUNIT));
    glActiveTextureARB(GL_TEXTURE0_ARB);
  end;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glTranslatef(sprite.x, sprite.y, sprite.z);
  glRotatef(sprite.mo.angle / (ANGLE_MAX / 360.0) - 90.0, 0.0, 1.0, 0.0);

  // JVAL
  // Draw light effects (only if not invulnerability)
  if uselightboost then
    if not lightdeflumppresent then
      if players[displayplayer].fixedcolormap <> 32 then
      // JVAL: Use old color effects only if LIGHTDEF lump not found
        if sprite.flags and GLS_LIGHT <> 0 then
        begin
          if sprite.flags and GLS_WHITELIGHT <> 0 then
            gld_SetUplight(1.0, 1.0, 1.0)
          else if sprite.flags and GLS_REDLIGHT <> 0 then
            gld_SetUplight(1.0, 0.0, 0.0)
          else if sprite.flags and GLS_GREENLIGHT <> 0 then
            gld_SetUplight(0.0, 1.0, 0.0)
          else if sprite.flags and GLS_BLUELIGHT <> 0 then
            gld_SetUplight(0.0, 0.0, 1.0)
          else if sprite.flags and GLS_YELLOWLIGHT <> 0 then
            gld_SetUplight(1.0, 1.0, 0.0);
          glBegin(GL_TRIANGLE_STRIP);
            glTexCoord2f(0.0, 0.0);
            glVertex3f(2.0 * sprite.x1, 2.0 * sprite.y1, 0.01);
            glTexCoord2f(1.0, 0.0);
            glVertex3f(2.0 * sprite.x2, 2.0 * sprite.y1, 0.01);
            glTexCoord2f(0.0, 1.0);
            glVertex3f(2.0 * sprite.x1, 2.0 * sprite.y2, 0.01);
            glTexCoord2f(1.0, 1.0);
            glVertex3f(2.0 * sprite.x2, 2.0 * sprite.y2, 0.01);
          glEnd;
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
          glAlphaFunc(GL_GEQUAL, 0.5);
        end;

  for i := 0 to sprite.voxels.Count - 1 do
    gld_DrawVoxel(sprite, sprite.voxels.Numbers[i]);

  glPopMatrix;

  if gl_uselightmaps then
  begin
    glActiveTextureARB(GL_TEXTURE1_ARB);
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glActiveTextureARB(GL_TEXTURE0_ARB);
  end;

  if sprite.flags and (GLS_SHADOW or GLS_TRANSPARENT or GLS_ADDITIVE or GLS_SUBTRACTIVE) <> 0 then
  begin
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glAlphaFunc(GL_GEQUAL, 0.5);
    if sprite.flags and GLS_SUBTRACTIVE <> 0 then
      glBlendEquation(GL_FUNC_ADD);
  end;
  glColor3f(1.0, 1.0, 1.0);
end;

procedure gld_DrawSprite(sprite: PGLSprite);
var
  haswhitefog: boolean; // JVAL: Mars fog sectors
begin
  haswhitefog := Psubsector_t(sprite.mo.subsector).sector.renderflags and SRF_FOG <> 0;

  if gl_drawmodels and (sprite.models <> nil) then
  begin
    if haswhitefog then gld_StartWhiteFog;  // JVAL: Mars fog sectors
    gld_DrawModels(sprite);
    if haswhitefog then gld_StartFog; // JVAL: Mars fog sectors
    exit;
  end;

  if gl_drawvoxels and (sprite.voxels <> nil) then
  begin
    if haswhitefog then gld_StartWhiteFog;  // JVAL: Mars fog sectors
    gld_DrawVoxels(sprite);
    if haswhitefog then gld_StartFog; // JVAL: Mars fog sectors
    exit;
  end;

  if haswhitefog then gld_StartWhiteFog;  // JVAL: Mars fog sectors

  if gl_uselightmaps then
  begin
    glActiveTextureARB(GL_TEXTURE1_ARB);
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    glTranslatef(sprite.x * MAP_COEFF / (LIGHTMAPSIZEX * LIGHTMAPUNIT),
                 sprite.y * MAP_COEFF / (LIGHTMAPSIZEY * LIGHTMAPUNIT),
                 sprite.z * MAP_COEFF / (LIGHTMAPSIZEZ * LIGHTMAPUNIT));
    glActiveTextureARB(GL_TEXTURE0_ARB);
  end;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glTranslatef(sprite.x, sprite.y, sprite.z);
  glRotatef(inv_yaw, 0.0, 1.0, 0.0);

  // JVAL
  // Draw light effects (only if not invulnerability)
  if uselightboost then
    if not lightdeflumppresent then
      if players[displayplayer].fixedcolormap <> 32 then
      // JVAL: Use old color effects only if LIGHTDEF lump not found
        if sprite.flags and GLS_LIGHT <> 0 then
        begin
          if sprite.flags and GLS_WHITELIGHT <> 0 then
            gld_SetUplight(1.0, 1.0, 1.0)
          else if sprite.flags and GLS_REDLIGHT <> 0 then
            gld_SetUplight(1.0, 0.0, 0.0)
          else if sprite.flags and GLS_GREENLIGHT <> 0 then
            gld_SetUplight(0.0, 1.0, 0.0)
          else if sprite.flags and GLS_BLUELIGHT <> 0 then
            gld_SetUplight(0.0, 0.0, 1.0)
          else if sprite.flags and GLS_YELLOWLIGHT <> 0 then
            gld_SetUplight(1.0, 1.0, 0.0);
          glBegin(GL_TRIANGLE_STRIP);
            glTexCoord2f(0.0, 0.0);
            glVertex3f(2.0 * sprite.x1, 2.0 * sprite.y1, 0.01);
            glTexCoord2f(1.0, 0.0);
            glVertex3f(2.0 * sprite.x2, 2.0 * sprite.y1, 0.01);
            glTexCoord2f(0.0, 1.0);
            glVertex3f(2.0 * sprite.x1, 2.0 * sprite.y2, 0.01);
            glTexCoord2f(1.0, 1.0);
            glVertex3f(2.0 * sprite.x2, 2.0 * sprite.y2, 0.01);
          glEnd;
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
          glAlphaFunc(GL_GEQUAL, 0.5);
        end;

  gld_BindPatch(sprite.gltexture, sprite.cm);

  if sprite.flags and GLS_SHADOW <> 0 then
  begin
    glBlendFunc(GL_DST_COLOR, GL_ONE_MINUS_SRC_ALPHA);
    glAlphaFunc(GL_GEQUAL, 0.1);
    glColor4f(0.2, 0.2, 0.2, 0.33);
  end
  else
  begin
    if sprite.flags and GLS_TRANSPARENT <> 0 then
    begin
      gld_StaticLightAlpha(sprite.light, sprite.alpha);
      glAlphaFunc(GL_GEQUAL, 0.01);
    end
    else if sprite.flags and GLS_ADDITIVE <> 0 then
    begin
      gld_StaticLightAlpha(sprite.light, sprite.alpha);
      glAlphaFunc(GL_GEQUAL, 0.01);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    end
    else if sprite.flags and GLS_SUBTRACTIVE <> 0 then
    begin
      gld_StaticLightAlpha(sprite.light, sprite.alpha);
      glAlphaFunc(GL_GEQUAL, 0.01);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      glBlendEquation(GL_FUNC_REVERSE_SUBTRACT);
    end
    else
      gld_StaticLight(sprite.light);
  end;
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(sprite.ul, sprite.vt);
    glVertex3f(sprite.x1, sprite.y1, 0.0);
    glTexCoord2f(sprite.ur, sprite.vt);
    glVertex3f(sprite.x2, sprite.y1, 0.0);
    glTexCoord2f(sprite.ul, sprite.vb);
    glVertex3f(sprite.x1, sprite.y2, 0.0);
    glTexCoord2f(sprite.ur, sprite.vb);
    glVertex3f(sprite.x2, sprite.y2, 0.0);
  glEnd;

  glPopMatrix;

  if gl_uselightmaps then
  begin
    glActiveTextureARB(GL_TEXTURE1_ARB);
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glActiveTextureARB(GL_TEXTURE0_ARB);
  end;

  if sprite.flags and (GLS_SHADOW or GLS_TRANSPARENT or GLS_ADDITIVE or GLS_SUBTRACTIVE) <> 0 then
  begin
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glAlphaFunc(GL_GEQUAL, 0.5);
    if sprite.flags and GLS_SUBTRACTIVE <> 0 then
      glBlendEquation(GL_FUNC_ADD);
  end;
  glColor3f(1.0, 1.0, 1.0);

  if haswhitefog then gld_StartFog; // JVAL: Mars fog sectors
end;

const
  COS16TABLE: array[0..16] of float = (
    1.0000000000,
    0.9238795042,
    0.7071067691,
    0.3826834261,
   -0.0000000000,
   -0.3826834261,
   -0.7071067691,
   -0.9238795042,
   -1.0000000000,
   -0.9238795042,
   -0.7071067691,
   -0.3826834261,
    0.0000000000,
    0.3826834261,
    0.7071067691,
    0.9238795042,
    1.0000000000
   );

  SIN16TABLE: array[0..16] of float = (
    0.0000000000,
    0.3826834261,
    0.7071067691,
    0.9238795042,
    1.0000000000,
    0.9238795042,
    0.7071067691,
    0.3826834261,
   -0.0000000000,
   -0.3826834261,
   -0.7071067691,
   -0.9238795042,
   -1.0000000000,
   -0.9238795042,
   -0.7071067691,
   -0.3826834261,
    0.0000000000
   );


//
//  gld_DrawDLight()
//  JVAL: Draw a single dynamic light
//
procedure gld_DrawDLight(const pdls: Pdlsortitem_t);
var
  i: integer;
  sz: float;
begin
  glPushMatrix;
  glTranslatef(pdls.x, pdls.y, pdls.z);
  glRotatef(inv_yaw, 0.0, 1.0, 0.0);

  sz := pdls.l.radius;
//  if GL_CheckVisibility(pdls.x, pdls.y, pdls.z, 2 * sz) then
  begin
    glBegin(GL_TRIANGLE_FAN);
      glColor4f(pdls.l.r * 0.2, pdls.l.g * 0.2, pdls.l.b * 0.2, 0.1);
      glVertex3f(0.0, 0.0, 0.05);
      glColor4f(0.0, 0.0, 0.0, 0.1);
      for i := 0 to 16 do
        glVertex3f(sz * COS16TABLE[i], sz * SIN16TABLE[i], 0.05);
    glEnd;
  end;

  glPopMatrix;
end;

//
//  gld_SortDlights()
//  JVAL: Sort the dynamic lights according to square distance of camera
//        (note: closer light is first!)
//
procedure gld_SortDlights;

  procedure qsort(l, r: Integer);
  var
    i, j: Integer;
    tmp: dlsortitem_t;
    squaredist: float;
  begin
    repeat
      i := l;
      j := r;
      squaredist := dlbuffer[(l + r) shr 1].squaredist;
      repeat
        while dlbuffer[i].squaredist < squaredist do
          inc(i);
        while dlbuffer[j].squaredist > squaredist do
          dec(j);
        if i <= j then
        begin
          tmp := dlbuffer[i];
          dlbuffer[i] := dlbuffer[j];
          dlbuffer[j] := tmp;
          inc(i);
          dec(j);
        end;
      until i > j;
      if l < j then
        qsort(l, j);
      l := i;
    until i >= r;
  end;

begin
  if numdlitems > 0 then
    qsort(0, numdlitems - 1);
end;

//
//  gld_DrawDLights()
//  JVAL: Draw the marked dynamic lights
//
procedure gld_DrawDLights;
var
  pdls: Pdlsortitem_t;
  lastitem: dlsortitem_t;
begin
  if not uselightboost then
    exit;

  if numdlitems = 0 then
    exit;

  glMatrixMode(GL_MODELVIEW);
  glDepthMask(False);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_ALPHA_TEST);
  glBlendFunc(GL_ONE, GL_ONE);

  ZeroMemory(@lastitem, SizeOf(dlsortitem_t));
  // Draw each light in order
  pdls := @dlbuffer[numdlitems];
  while integer(pdls) <> integer(dlbuffer) do
  begin
    dec(pdls);
    if (pdls.l <> lastitem.l) or
       (pdls.x <> lastitem.x) or
       (pdls.y <> lastitem.y) or
       (pdls.z <> lastitem.z) then
    begin
      gld_DrawDLight(pdls);
      lastitem := pdls^;
    end;
  end;

  glDepthMask(True);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_ALPHA_TEST);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

end;

procedure gld_AddSprite(vspr: Pvissprite_t);
var
  pSpr: Pmobj_t;
  sprite: GLSprite;
  voff, hoff: float;
  tex: PGLTexture;
  sec: Psector_t;
begin
  pSpr := vspr.mo;
  sprite.scale := vspr.scale;
  if pSpr.frame and FF_FULLBRIGHT <> 0 then
    sprite.light := 1.0
  else
  begin
    sec := Psubsector_t(pSpr.subsector).sector;
    if sec.midsec < 0 then
      sprite.light := gld_CalcLightLevel(sec.lightlevel + (extralight shl 5))
    else if P_3dFloorNumber(pSpr) = 0 then
      sprite.light := gld_CalcLightLevel(sectors[sec.midsec].lightlevel + (extralight shl 5))
    else
      sprite.light := gld_CalcLightLevel(sec.lightlevel + (extralight shl 5));
  end;
  sprite.cm := Ord(CR_LIMIT) + ((pSpr.flags and MF_TRANSLATION) shr MF_TRANSSHIFT);
  sprite.gltexture := gld_RegisterPatch(vspr.patch + firstspritelump, sprite.cm);
  if sprite.gltexture = nil then
    exit;

  tex := sprite.gltexture;

  sprite.alpha := 0.0;
  sprite.flags := 0;
  {$IFDEF STRIFE}
  if pSpr.flags and MF_SHADOW <> 0 then
  begin
    sprite.flags := sprite.flags or GLS_TRANSPARENT;
    sprite.alpha := 33 / 100;
  end;
  if pSpr.flags and MF_MVIS <> 0 then
  begin
    sprite.flags := sprite.flags or GLS_TRANSPARENT;
    sprite.alpha := 66 / 100;
  end;
  {$ELSE}
  if pSpr.flags and MF_SHADOW <> 0 then
    sprite.flags := GLS_SHADOW;
  {$ENDIF}
  if pSpr.flags_ex and MF_EX_TRANSPARENT <> 0 then // JVAL -> alpha here!!!!
  begin
    sprite.flags := sprite.flags or GLS_TRANSPARENT;
    sprite.alpha := tran_filter_pct / 100;
  end;
  if pSpr.renderstyle = mrs_translucent then
  begin
    sprite.flags := sprite.flags or GLS_TRANSPARENT;
    sprite.alpha := pSpr.alpha / FRACUNIT;
  end
  else if pSpr.renderstyle = mrs_add then
  begin
    sprite.flags := sprite.flags or GLS_ADDITIVE;
    sprite.alpha := pSpr.alpha / FRACUNIT;
  end
  else if pSpr.renderstyle = mrs_subtract then
  begin
    sprite.flags := sprite.flags or GLS_SUBTRACTIVE;
    sprite.alpha := pSpr.alpha / FRACUNIT;
  end;

  if pSpr.flags_ex and MF_EX_LIGHT <> 0 then
  begin
    if pSpr.flags_ex and MF_EX_WHITELIGHT <> 0 then
      sprite.flags := sprite.flags or GLS_WHITELIGHT;
    if pSpr.flags_ex and MF_EX_REDLIGHT <> 0 then
      sprite.flags := sprite.flags or GLS_REDLIGHT;
    if pSpr.flags_ex and MF_EX_GREENLIGHT <> 0 then
      sprite.flags := sprite.flags or GLS_GREENLIGHT;
    if pSpr.flags_ex and MF_EX_BLUELIGHT <> 0 then
      sprite.flags := sprite.flags or GLS_BLUELIGHT;
    if pSpr.flags_ex and MF_EX_YELLOWLIGHT <> 0 then
      sprite.flags := sprite.flags or GLS_YELLOWLIGHT;
  end;

  sprite.dlights := vspr.mo.state.dlights;
  sprite.models := vspr.mo.state.models;
  sprite.voxels := vspr.mo.state.voxels;
  sprite.mo := pSpr;
  sprite.aproxdist := P_AproxDistance(pSpr.x - viewx, pSpr.y - viewy);

  sprite.x := -pSpr.x / MAP_SCALE;
  sprite.y :=  (pSpr.z{$IFNDEF HERETIC} - pSpr.floorclip{$ENDIF}) / MAP_SCALE;
  sprite.z :=  pSpr.y / MAP_SCALE;

  sprite.vt := 0.0;
  sprite.vb := tex.height / tex.tex_height;
  if vspr.flip then
  begin
    sprite.ul := 0.0;
    sprite.ur := tex.width / tex.tex_width;
  end
  else
  begin
    sprite.ul := tex.width / tex.tex_width;
    sprite.ur := 0.0;
  end;
  hoff := tex.leftoffset / MAP_COEFF;
  voff := tex.topoffset / MAP_COEFF;
  sprite.x1 := hoff - (tex.realtexwidth / MAP_COEFF);
  sprite.x2 := hoff;
  sprite.y1 := voff;
  sprite.y2 := voff - (tex.realtexheight / MAP_COEFF);

  if vspr.infoscale <> FRACUNIT then
  begin
    sprite.x1 := sprite.x1 * vspr.infoscale / FRACUNIT;
    sprite.x2 := sprite.x2 * vspr.infoscale / FRACUNIT;
    sprite.y1 := sprite.y1 * vspr.infoscale / FRACUNIT;
    sprite.y2 := sprite.y2 * vspr.infoscale / FRACUNIT;
  end;

  if (sprite.y2 < 0) and (vspr.mobjflags and (MF_SPAWNCEILING or MF_FLOAT or MF_MISSILE or MF_NOGRAVITY) = 0) then
  begin
    sprite.y1 := sprite.y1 - sprite.y2;
    sprite.y2 := 0.0;
    sprite.flags := sprite.flags or GLS_CLIPPED;
  end;

  if gld_drawinfo.num_sprites >= gld_drawinfo.max_sprites then
  begin
    gld_drawinfo.max_sprites := gld_drawinfo.max_sprites + 128;
    gld_drawinfo.sprites := Z_Realloc(gld_drawinfo.sprites, gld_drawinfo.max_sprites * SizeOf(GLSprite), PU_LEVEL, nil);
  end;
  gld_AddDrawItem(GLDIT_SPRITE, gld_drawinfo.num_sprites);
  gld_drawinfo.sprites[gld_drawinfo.num_sprites] := sprite;
  inc(gld_drawinfo.num_sprites);

  // JVAL: Mark the dynamic lights of the sprite
  if sprite.aproxdist < DLIGHTSDRAWRANGE then
    gld_MarkDLights(@sprite);

  if gl_drawshadows and (sprite.dlights = nil) then
    if sprite.aproxdist < SHADOWSDRAWRANGE then
      if pSpr.flags2_ex and MF2_EX_DONOTRENDERSHADOW = 0 then // JVAL: VERSION 204
        gld_MarkDShadow(@sprite);
end;

(*****************
 *               *
 * Draw          *
 *               *
 *****************)

procedure gld_StartFog;
var
  FogColor: array[0..3] of TGLfloat; // JVAL: set blue fog color if underwater
begin
  if use_fog then
    if players[displayplayer].fixedcolormap = 0 then
    begin
{$IFDEF DOOM_OR_STRIFE}
      if customcolormap <> nil then
      begin
        glFogf(GL_FOG_DENSITY, customcolormap.fog_density * fog_density / 1000.0);

        FogColor[0] := customcolormap.fog_r;
        FogColor[1] := customcolormap.fog_g;
        FogColor[2] := customcolormap.fog_b;
        FogColor[3] := 0.0;
      end
      else
      begin
        glFogf(GL_FOG_DENSITY, fog_density / 1000.0);

        FogColor[0] := 0.0;
        FogColor[1] := 0.0;
        FogColor[2] := 0.0;
        FogColor[3] := 0.0;
      end;

      glFogfv(GL_FOG_COLOR, @FogColor);
{$ENDIF}
{$IFDEF HERETIC}
      glFogf(GL_FOG_DENSITY, fog_density / 1000.0);

      FogColor[0] := 0.0;
      FogColor[1] := 0.0;
      FogColor[2] := 0.0;
      FogColor[3] := 0.0;

      glFogfv(GL_FOG_COLOR, @FogColor);
{$ENDIF}
{$IFDEF HEXEN}
      glFogf(GL_FOG_DENSITY, fog_density / 1000.0);
      if LevelUseFog then
      begin
        FogColor[0] := 1.0;
        FogColor[1] := 1.0;
        FogColor[2] := 1.0;
        FogColor[3] := 0.0;
      end
      else
      begin
        FogColor[0] := 0.0;
        FogColor[1] := 0.0;
        FogColor[2] := 0.0;
        FogColor[3] := 0.0;
      end;

      glFogfv(GL_FOG_COLOR, @FogColor);
{$ENDIF}

      glEnable(GL_FOG);
      exit;
    end;

  glDisable(GL_FOG);
end;

procedure gld_StartWhiteFog;
var
  FogColor: array[0..3] of TGLfloat; // JVAL: set blue fog color if underwater
begin
  if use_white_fog then
    if players[displayplayer].fixedcolormap = 0 then
    begin
      glFogf(GL_FOG_DENSITY, white_fog_density / 1000.0);

  {$IFDEF DOOM_OR_STRIFE}
      if customcolormap <> nil then
      begin
        FogColor[0] := (customcolormap.fog_r + 1.0) / 2.0;
        FogColor[1] := (customcolormap.fog_g + 1.0) / 2.0;
        FogColor[2] := (customcolormap.fog_b + 1.0) / 2.0;
        FogColor[3] := 0.0;
      end
      else
      begin
        FogColor[0] := 1.0;
        FogColor[1] := 1.0;
        FogColor[2] := 1.0;
        FogColor[3] := 0.0;
      end;
  {$ENDIF}
  {$IFDEF HERETIC_OR_HEXEN}
      FogColor[0] := 1.0;
      FogColor[1] := 1.0;
      FogColor[2] := 1.0;
      FogColor[3] := 0.0;
  {$ENDIF}

      glFogfv(GL_FOG_COLOR, @FogColor);
      glEnable(GL_FOG);
      exit;
    end;

  glDisable(GL_FOG);
end;

procedure gld_DrawWalls(const wallrange: integer; const fblend: boolean);
var
  i, j, k, count: integer;
  pglitem: PGLDrawItem;
begin
  for i := gld_drawinfo.num_drawitems downto 0 do
  begin
    pglitem := @gld_drawinfo.drawitems[i];
    if pglitem.itemtype = GLDIT_WALL then
    begin
      count := 0;
      for k := GLDWF_TOP to wallrange do
      begin
        if count >= pglitem.itemcount then
          continue;
        if gl_drawsky and (k >= GLDWF_SKY) then
        begin
          gld_PauseLightmap;
          if gl_shared_texture_palette then
            glDisable(GL_SHARED_TEXTURE_PALETTE_EXT);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          glEnable(GL_TEXTURE_GEN_Q);
          glDisable(GL_FOG);
          glColor4fv(@gl_whitecolor);
        end;
        for j := pglitem.itemcount - 1 downto 0 do
          if gld_drawinfo.walls[j + pglitem.firstitemindex].flag = k then
          begin
            {$IFDEF DEBUG}
            inc(rendered_segs);
            {$ENDIF}
            inc(count);
            gld_DrawWall(@gld_drawinfo.walls[j + pglitem.firstitemindex], fblend);
          end;
        if gl_drawsky and (k >= GLDWF_SKY) then
        begin
          gld_ResumeLightmap;
          gld_StartFog;
          glDisable(GL_TEXTURE_GEN_Q);
          glDisable(GL_TEXTURE_GEN_T);
          glDisable(GL_TEXTURE_GEN_S);
          if gl_shared_texture_palette then
            glEnable(GL_SHARED_TEXTURE_PALETTE_EXT);
        end;
      end;
    end;
  end;
end;

procedure gld_DrawScene(player: Pplayer_t);
var
  i, j, k: integer;
  max_scale: fixed_t;
  wallrange: integer;
  pglitem: PGLDrawItem;
  wall: PGLWall;
  seg: PGLSeg;
begin
  if zaxisshift then
  begin
    wallrange := GLDWF_BOTFLUD;
    if not gl_stencilsky and dodrawsky then
    begin
      if pitch <= -35.0 then
        gld_DrawSky(true, false)
      else if pitch >= 35.0 then
        gld_DrawSky(false, true)
      else
        gld_DrawSky(true, true);
    end;
  end
  else
    wallrange := GLDWF_SKYFLIP;

  // Sort the dynamic lights
  gld_SortDlights;

  gl_uselightmaps := gl_uselightmaps and canuselightmaps;

  if gl_uselightmaps then
    gld_ActivateLightmap;

  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);

  gld_StartFog;

{$IFDEF DEBUG}
  rendered_visplanes := 0;
  rendered_segs := 0;
  rendered_vissprites := 0;
{$ENDIF}

  glTexCoordPointer(2, GL_FLOAT, 0, gld_texcoords);
  glVertexPointer(3, GL_FLOAT, 0, gld_vertexes);

  // Floors and ceilings
  glDisable(GL_BLEND);
  for i := gld_drawinfo.num_drawitems downto 0 do
  begin
    pglitem := @gld_drawinfo.drawitems[i];
    if pglitem.itemtype = GLDIT_FLAT then
    begin
      {$IFDEF DEBUG}
      rendered_visplanes := rendered_visplanes + pglitem.itemcount;
      {$ENDIF}
      for j := pglitem.itemcount - 1 downto 0 do
        gld_DrawFlat(@gld_drawinfo.flats[j + pglitem.firstitemindex]);
    end;
  end;
  glEnable(GL_BLEND);

  gld_DrawWalls(wallrange, false);

//  glCullFace(GL_BACK);
  // Walls
  // Sprites
  if gld_drawinfo.num_sprites > 1000 then
  begin
    for i := 0 to gld_drawinfo.num_sprites - 1 do
    begin
      {$IFDEF DEBUG}
      inc(rendered_vissprites);
      {$ENDIF}
      gld_DrawSprite(@gld_drawinfo.sprites[i]);
    end;
  end
  else for i := gld_drawinfo.num_drawitems downto 0 do
  begin
    pglitem := @gld_drawinfo.drawitems[i];
    if pglitem.itemtype = GLDIT_SPRITE then
    begin
      repeat
        max_scale := MAXINT;
        k := -1;
        for j := pglitem.itemcount - 1 downto 0 do
          if gld_drawinfo.sprites[j + pglitem.firstitemindex].scale < max_scale then
          begin
            max_scale := gld_drawinfo.sprites[j + pglitem.firstitemindex].scale;
            k := j + pglitem.firstitemindex;
          end;
        if k >= 0 then
        begin
          {$IFDEF DEBUG}
          inc(rendered_vissprites);
          {$ENDIF}
          gld_DrawSprite(@gld_drawinfo.sprites[k]);
          gld_drawinfo.sprites[k].scale := MAXINT;
        end;
      until max_scale = MAXINT;
    end;
  end;

  gld_DrawWalls(GLDWF_BOTFLUD, true);

  if gl_uselightmaps then
    gld_DeActivateLightmap;

  glDisable(GL_FOG);

  gld_DrawDLights;

  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);

  if zaxisshift then
  begin
    if gl_drawsky and dodrawsky then
    begin

      if gl_stencilsky then
      begin
        glClear(GL_STENCIL_BUFFER_BIT); // Clear the stencil buffer the first time
        glEnable(GL_STENCIL_TEST);      // Turn on the stencil buffer test
        glDisable(GL_TEXTURE_2D);
        glColor4f(1.0, 1.0, 1.0, 1.0);
        glStencilFunc(GL_ALWAYS, 1, 1); // Setup the stencil buffer to write
        glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);  // a 1 everywhere the plane is.

        for j := 0 to gld_drawinfo.num_walls - 1 do
          if gld_drawinfo.walls[j].flag >= GLDWF_SKY then
          begin
            wall := @gld_drawinfo.walls[j];
            seg := wall.glseg;
            glBegin(GL_TRIANGLE_STRIP);
              glVertex3f(seg.x1, wall.ytop, seg.z1);
              glVertex3f(seg.x1, wall.ybottom, seg.z1);
              glVertex3f(seg.x2, wall.ytop, seg.z2);
              glVertex3f(seg.x2, wall.ybottom, seg.z2);
            glEnd;
          end;

        glStencilFunc(GL_EQUAL, 1, 1);          // Set the stencil buffer to update
        glStencilOp(GL_KEEP, GL_KEEP, GL_INCR); // only where it finds a 1 from the plane and increment the buffer

        glEnable(GL_TEXTURE_2D);

        glDisable(GL_DEPTH_TEST);
        if pitch <= -35.0 then
          gld_DrawSky(true, false)
        else if pitch >= 35.0 then
          gld_DrawSky(false, true)
        else
          gld_DrawSky(true, true);

        glEnable(GL_DEPTH_TEST);
        glDisable(GL_STENCIL_TEST); // Turn off the stencil buffer test

      end;
    end;
  end;

{$IFDEF DEBUG}
  printf('rendered_visplanes := %d'#13#10'rendered_segs := %d'#13#10'rendered_vissprites := %d'#13#10#13#10,
        [rendered_visplanes, rendered_segs, rendered_vissprites]);
{$ENDIF}

end;

{$IFDEF DOOM}
procedure gld_SkyTextureHack;
var
  i: integer;
  s, l: integer;
begin
  for i := 0 to numsectors - 1 do
    if sectors[i].sky and PL_SKYFLAT <> 0 then
    begin
      l := sectors[i].sky and not PL_SKYFLAT;
      if l >= 0 then
        if l < numlines then
        begin
          s := lines[l].sidenum[0];
          if s >= 0 then
            if s < numsides then
            begin
              skytexture := sides[s].toptexture;
              exit;
            end;
        end;
    end;
end;
{$ENDIF}

procedure gld_PreprocessLevel;
begin
  // preload graphics
  // JVAL
  // Precache if we have external textures
  {$IFDEF DOOM}
  gld_SkyTextureHack;
  {$ENDIF}
  if precache or externalpakspresent then
    gld_Precache;
  gld_PreprocessSectors;
  gld_PreprocessSegs;
  ZeroMemory(@gld_drawinfo, SizeOf(GLDrawInfo));
  glTexCoordPointer(2, GL_FLOAT, 0, gld_texcoords);
  glVertexPointer(3, GL_FLOAT, 0, gld_vertexes);
end;

procedure gld_FreeSectorLists;
var
  i: integer;
begin
  for i := 0 to numsectors - 1 do
    if (sectorloops[i].list <> 0) and (sectorloops[i].list <> GL_BAD_LIST) then
      glDeleteLists(sectorloops[i].list, 1);
end;

procedure gld_CleanMemory;
begin
  gld_FreeSectorLists;
  gld_CleanTextures;
  gld_CleanPatchTextures;
  gld_CleanModelTextures;
end;

procedure R_ShutDownOpenGL;
begin
  if gl_initialized then
  begin
    gld_SkyDone;
    R_DynamicLightsDone;
    gld_DynamicShadowsDone;
    gld_ModelsDone;
    gld_VoxelsDone;
    gld_LightmapDone;
    gld_ClipperDone;
    gld_AmbientDone;
    gld_ShutDownAutomap;
  end;
end;

end.

