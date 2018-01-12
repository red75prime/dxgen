use cgmath::*;
use crossbeam;
use rand;
use rand::Rng;
use utils::{self, v3};
use ncollide as nc;
use ncollide::broad_phase::BroadPhase;
use ncollide::bounding_volume;
use nalgebra as na;

pub type V3 = Vector3<f32>;

#[derive(Clone)]
pub struct CubeState {
    pub pos: V3,
    pub rot: Quaternion<f32>,
    pub spd: V3,
    pub color: V3,
    pub blink: bool,
    pub rot_axis: V3,
    pub rot_spd: f32,
    pub p_accel: V3, // acceleration caused by penetration
    pub p_time: f32,
}

#[derive(Clone)]
pub struct State {
    pub tick: f64,
    pub cubes: Vec<CubeState>,
    pub light_pos: V3,
}

const CUBE_SPAN : f32 = 100.;

fn min_f32_pair<T: Iterator<Item=(f32,P)>, P: Copy>(mut it: T) -> Option<(f32, P)> {
    match it.next() {
        None => None,
        Some((v, p)) => {
            let mut mv = v;
            let mut mp = p;
            for (v, p) in it {
                if mv.is_nan() || mv>v {
                    mv = v;
                    mp = p;
                }
            };
            if mv.is_nan() {
                None
            } else {
                Some((mv, mp))
            }
        }
    }
}

#[test]
fn min_f32_pair_test() {
    assert_eq!(min_f32_pair::<_,()>([].iter().cloned()), None);
    assert_eq!(min_f32_pair([(0., ()), (1., ())].iter().cloned()), Some((0., ())));
    assert_eq!(min_f32_pair([(1., ()), (0., ())].iter().cloned()), Some((0., ())));
    assert_eq!(min_f32_pair([(1., ()), (0., ()), (1., ())].iter().cloned()), Some((0., ())));
    assert_eq!(min_f32_pair([(0., ()), (0., ()), (1., ())].iter().cloned()), Some((0., ())));
    assert_eq!(min_f32_pair([(0., ()), (0., ()), (0., ())].iter().cloned()), Some((0., ())));
    assert_eq!(min_f32_pair([(0./0., ()), (0., ()), (1., ())].iter().cloned()), Some((0., ())));
    assert_eq!(min_f32_pair([(0./0., ()), (2., ()), (1., ())].iter().cloned()), Some((1., ())));
    assert_eq!(min_f32_pair([(0./0., ())].iter().cloned()), None);
}

#[derive(Debug)]
struct CollisionParms {
    normal_bounciness: f32,
    min_speed: f32,
    friction: f32,
}

// Distance from point p to the oriented plane (pp, n), positive if point on the same side as a normal 
// "Oriented plane" isn't a proper mathematical term, I don't know how it is commonly called. 
#[inline]
fn plane_dist(p: V3, (pp, n): (V3, V3)) -> f32 {
    (p-pp).dot(n)
}

// returns distance from point to the nearest oriented plane.
// Plane's normal points into allowed volume, points outside allowed volume is nearer to the plane, than points inside allowed volume. 
fn nearest_plane(p: V3, plns: &[(V3,V3)]) -> (f32, (V3, V3)) {
    assert!(plns.len()>0);
    if let Some(dppn) = min_f32_pair(plns.iter().cloned().map(|plane|(plane_dist(p, plane), plane))) {
        dppn // Distance, Plane Point, Normal 
    } else {
        // if plns slice is non-empty, then the only reason for min_f32_pair to return None is NaN somewhere  
        panic!("nearest_plane: NaNs! NaNs everywhere!");
    }
}

// roots of quadratic equation
#[derive(Debug, Clone, Copy)]
enum QERoot {
    NoRoot, 
    OneRoot(f32),
    TwoRoots(f32, f32),
}
use self::QERoot::{NoRoot, OneRoot, TwoRoots};

impl PartialEq<QERoot> for QERoot {
    fn eq(&self, other: &QERoot) -> bool {
        match (*self, *other) {
            (NoRoot, NoRoot) => true,
            (OneRoot(s1), OneRoot(o1)) => s1 == o1,
            (TwoRoots(s1, s2), TwoRoots(o1, o2)) => (s1==o1 && s2==o2) || (s1==o2 && s2==o1), // order independent equality. For test, mostly
            _ => false, 
        }
    } 
}

// solves quadratic equation a*x*x + b*x + c = 0
// TwoRoots are in unspecified order
fn qe_roots(a: f32, b: f32, c: f32) -> QERoot {
    if a == 0. {
        // linear equation
        OneRoot(-c/b)
    } else {
        let detsq = b*b - 4.*a*c;
        if detsq < 0. {
            NoRoot // complex root, no physical sense 
        } else if detsq == 0. {
            OneRoot(-b/(2.*a))
        } else {
            let det = detsq.sqrt();
            // avoid close subtraction
            if b <= 0. {
                TwoRoots((-b+det)/(2.*a), 2.*c/(-b+det))
            } else {
                TwoRoots((-b-det)/(2.*a), 2.*c/(-b-det))
            }
        }
    }
}

#[test]
fn qe_roots_test() {
    assert_eq!(qe_roots(1., 0., 0.), OneRoot(0.));
    assert_eq!(qe_roots(1., 0., 1.), NoRoot);
    assert_eq!(qe_roots(1., 0., -1.), TwoRoots(-1., 1.));
    assert_eq!(qe_roots(2., 5., -25.), TwoRoots(-5., 2.5));
    assert_eq!(qe_roots(2., -5., 0.), TwoRoots(0., 2.5));
    assert_eq!(qe_roots(0., 5., -25.), OneRoot(5.));
}


// Returns smallest of roots, which is greater than zero.
// NaNs... are a possibility. In this context (collisions) 
// it makes sense to ignore them, as they should be caused only
// by sphere which touches the plane and moves parallel to it,
// so sphere isn't really colliding with plane 
fn nearest_physical_time(qer: QERoot) -> Option<f32> {
    match qer {
        NoRoot => None,
        OneRoot(r) => {
            if r.is_nan() || r <= 0. { None } else { Some(r) }
        },
        TwoRoots(r1, r2) => {
            if r1.is_nan() || r1 <= 0. {
                if r2.is_nan() || r2 <= 0. {
                    // both NaNs or less then zero
                    None
                } else {
                    // r1 isn't acceptable, r2 is
                    Some(r2)
                }
            } else if r2.is_nan() || r2 <= 0. {
                // r1 is acceptable, r2 isn't
                Some(r1)
            } else {
                // both are acceptable
                Some(f32::min(r1, r2))
            }
        }
    }
}

static TOUCH_TOLERANCE: f32 = 0.0001;
static EPS: f32 = 0.000001;

// p - initial position
// v - initial speed
// a - acceleration
// r - sphere radius
// (pp, n) - point on plane, normal pointing into allowed volume
fn time_to_hit(p: V3, v: V3, a: V3, r: f32, (pp, n): (V3, V3)) -> Option<f32> {
    // we care only about motion along normal, so let's project everything onto it.
    let nd = (p-pp).dot(n); // distance from center of sphere to the plane
    let nv = v.dot(n); // speed along nornal (positive - into allowed space)
    if (nd-r).abs() < TOUCH_TOLERANCE && nv < EPS {
        // sphere is moving alongside plane. skip it
        return None;
    };
    if nd-r < 0. {
        // sphere is in restricted space, skip this plane
        return None;
    }
    let na = a.dot(n); // acceleration (positive - into allowed space)
    // motion equation x = na*t*t/2 + nv*t + nd
    // solve na*t*t/2 + nv*t + (nd-r) = 0
    // and return physical root
    nearest_physical_time(qe_roots(na/2., nv, nd-r))
}

fn reflect(i: V3, n: V3) -> V3 {
    i-n*(n.dot(i)*2.)
}

// dt - time step
// p - initial position
// v - initial speed
// a - acceleration
// r - radius
// plns - planes (point, interior normal)
// returns time to collision or dt, position at collision point, speed after collision  
fn sphere_planes_collision(cp: &CollisionParms, dt: f32, p: V3, v: V3, a: V3, r: f32, plns: &[(V3,V3)]) -> (f32, V3, V3, Option<(V3,V3)>) {
    // recovery step if sphere is penetrating some plane(s)
    // TODO: do something more intelligent. 
    let (d, (pp, n)) = nearest_plane(p, plns);
    if d+TOUCH_TOLERANCE < r {
        // distance to the nearest plane is less than sphere radius
        let pcorr = p + (r - d)*n; // move sphere into allowed volume (possibly), and be done with it for this step
        let v = if n.dot(v) < 0. { reflect(v, n) } else { v };
        //println!("correction");
        return (dt, pcorr, v, Some((pp, n))); 
    };

    // Enforce constraints. Acceleration should not force sphere into disallowed volume
    let mut a = a;
    let mut v = v;
    let mut friction_mult = 0.;
    for plane in plns.iter().cloned() {
        let (_, n) = plane;
        if (plane_dist(p, plane)-r).abs() < TOUCH_TOLERANCE {
            let na = n.dot(a); // normal acceleration
            let nv = n.dot(v); // normal speed
            if na < 0. && nv < cp.min_speed {
                // sphere moves toward or parrallel to disallowed volume and 
                // acceleration forces sphere into disallowed volume,
                // correct it (apply reaction force)
                //println!("av correction before: a: {:?} v: {:?} na: {:?} nv:{:?} {:?}", a, v, na, nv, plane);
                a -= na*n;
                // correct speed to make the sphere move along the plane
                v -= nv*n;
                // add friction
                if v.magnitude() != 0. {
                    friction_mult += na.abs();
                }
                //println!("av correction after: a: {:?} v: {:?}", a, v);
            }
        }
    }
    // friction
    a -= v.normalize()*friction_mult*cp.friction;

    // Spere is inside allowed volume, let's check if it will hit anything in this time step
    // Checking final point is not enough. Top of the parabola could be outside allowed volume.
    // So we need to compute time-to-hit for all planes, select first one, then if it is greater than dt, we have no collisions
    if let Some((t,(pp,n))) = min_f32_pair(plns.iter().cloned().filter_map(|(pp, n)|time_to_hit(p, v, a, r, (pp, n)).map(|t|(t,(pp, n))))) {
        if t > dt {
            // collision after time step
            (dt, p+v*dt+a*dt*dt/2., v+a*dt, None)
        } else {
            let v1 = v+a*t; // speed at impact 
            let vb = reflect(v1, n)*cp.normal_bounciness;  // speed after bounce 
            (t, p+v*t+a*t*t/2., vb, Some((pp, n)))
        }
    } else {
        // No collisions. Allowed space is unbounded
        (dt, p+v*dt+a*dt*dt/2., v+a*dt, None)
    }
}

static BOUNCE_PLANES: [(V3,V3);6] = [
    (Vector3{x:0., y:0., z:0.}, Vector3{x:0., y:0., z:-1.}), // z = 0 plane
    (Vector3{x:0., y:0., z:-CUBE_SPAN}, Vector3{x:0., y:0., z:1.}), // z = -CUBE_SPAN
    (Vector3{x:CUBE_SPAN/2.,  y:0., z:0.}, Vector3{x:-1., y:0., z:0.}), // and so on
    (Vector3{x:-CUBE_SPAN/2., y:0., z:0.}, Vector3{x:1., y:0., z:0.}),
    (Vector3{x:0., y:CUBE_SPAN/2., z:0.}, Vector3{x:0., y:-1., z:0.}),
    (Vector3{x:0., y:-CUBE_SPAN/2., z:0.}, Vector3{x:0.1, y:0.9949874371, z:0.}),
];

#[derive(Debug, Clone, Copy)]
enum ObjType {
    Cam,
    Cub(usize),
}
use self::ObjType::{Cam, Cub};

impl State {
    pub fn new(object_count: u32) -> Self {
        let mut rng = rand::XorShiftRng::new_unseeded();
        let mut cubes = Vec::with_capacity(object_count as usize);
        for _ in 0 .. object_count {
            let x = rng.next_f32();
            let y = rng.next_f32();
            let z = rng.next_f32();
            let cube_state = CubeState {
                pos: v3((x - 0.5) * CUBE_SPAN,
                             (y - 0.5) * CUBE_SPAN,
                             (z - 1.) * CUBE_SPAN),
                color: v3(x, y, z),
                blink: false,
                rot: Quaternion::one(),
                spd: v3(rng.next_f32() - 0.5,
                             rng.next_f32() - 0.5,
                             rng.next_f32() - 0.5).normalize()*rng.next_f32()*1.,
                rot_axis: v3(rng.next_f32() - 0.5,
                             rng.next_f32() - 0.5,
                             rng.next_f32() - 0.5)
                              .normalize(),
                rot_spd: rng.next_f32()*30.0,
                p_accel: v3(0., 0., 0.),
                p_time: 0.,
            };
            cubes.push(cube_state);
        };
        State {
            tick: 0.0,
            cubes: cubes,
            light_pos: v3(0., 0., 0.),
        }
    }

    fn update_one(cube: &CubeState, dt: f32) -> CubeState {
        let r = 0.866;
        let a = v3(0., -9.8, 0.);
        let cp = CollisionParms {
            normal_bounciness: 0.99,
            min_speed: 0.05,
            friction: 0.2,
        };
        // bounce_planes is vec of tuples (point on plane, plane normal pointing inside allowed volume) 
        let mut pos = cube.pos;
        let mut spd = cube.spd;
        let mut rt = dt;
        if cube.p_time > 0. {
            let ptime = f32::min(rt, cube.p_time);
            let (t, p, v, _) = sphere_planes_collision(&cp, ptime, pos, spd, a+cube.p_accel, r, &BOUNCE_PLANES);
            rt -= t;
            pos = p;
            spd = v;
        }
        for _ in 0..5 { // prevent too many bounces in one time step 
            let (t, p, v, maybe_plane) = sphere_planes_collision(&cp, rt, pos, spd, a, r, &BOUNCE_PLANES);
            //println!("Step: {:?}, dt: {:?}, p: {:?}, v: {:?}, {:?}", i, t, p, v, maybe_plane);
            rt -= t;
            pos = p;
            spd = v;
            if rt <= 0. || maybe_plane.is_none() {
                break;
            }
        }

        CubeState {
            pos: pos,
            spd: spd,
            rot: Quaternion::from_axis_angle(cube.rot_axis, Rad(cube.rot_spd * dt)) * cube.rot,
            blink: false,
            p_accel: v3(0., 0., 0.),
            p_time: 0.,
            .. *cube
        }
    }

    pub fn update(&self, dt: f32, thread_cnt: u32, camera: Point3<f32>) -> Self {
        if dt == 0.0 {
          return (*self).clone();
        }
        // advance position of cubes
        // Uninitialized content of cubes will be immediately overwritten
        let mut cubes = unsafe {
            utils::uninitialized_vec(self.cubes.len())
        };
        if thread_cnt == 1 {
            for (dst, cube) in cubes.iter_mut().zip(&self.cubes[..]) {
                *dst = State::update_one(cube, dt);
            }
        } else {
            crossbeam::scope(|scope| {
                let chunk_len = (cubes.len()/thread_cnt as usize)+1;
                for (chunk, chunk_src) in cubes[..].chunks_mut(chunk_len).zip(self.cubes[..].chunks(chunk_len)) {
                    scope.spawn(move || {
                        for (c, cube) in chunk.iter_mut().zip(chunk_src) {
                            *c = State::update_one(cube, dt);
                        };
                    });
                };
            });
        }
        // creation of broad_phase searcher on each update should be less efficient than keeping it around and updating,
        // but test results proved otherwise. After several seconds updates became very slow.
        let mut broad_phase = nc::broad_phase::DBVTBroadPhase::new(0.1f32, true);
        let cuboid = nc::shape::Cuboid::new(na::Vector3::new(0.5, 0.5, 0.5));
        let bv = bounding_volume::bounding_sphere(&cuboid, &na::Vector3::new(camera.x, camera.y, camera.z));
        broad_phase.deferred_add(self.cubes.len(), bv, Cam);
        for (i, c) in self.cubes.iter().enumerate() {
            let bv = bounding_volume::bounding_sphere(&cuboid, &na::Vector3::new(c.pos.x, c.pos.y, c.pos.z));
            //println!("{:?}", bv);
            broad_phase.deferred_add(i, bv, Cub(i));
        }
        broad_phase.update(&mut |_, _| true, &mut |&i1, &i2, _|{
            match (i1, i2) {
                (Cam, Cam) => {},
                (Cub(i), Cam) | (Cam, Cub(i)) => {
                    let pos = cubes[i].pos;
                    let cpos = v3(camera.x, camera.y, camera.z);
                    if (pos-cpos).magnitude2() <= 3. {
                        let dc = pos - cpos;
                        if dc != v3(0., 0., 0.) {
                            let dp = dc.normalize();
                            let penetration = 1.732 - dc.magnitude();
                            cubes[i].p_accel += dp*penetration*100.;
                            cubes[i].p_time = 0.1;
                        }
                    }
                },
                (Cub(i1), Cub(i2)) => {
                    let c = (cubes[i1].pos - cubes[i2].pos).magnitude2() <= 3.;
                    if c {
                        let dc = cubes[i2].pos - cubes[i1].pos;
                        let dp = dc.normalize();
                        let ds = cubes[i2].spd - cubes[i1].spd;
                        let ci_spd = ds.dot(dp); 
                        if ci_spd < 0. {
                            // closing in
                            cubes[i1].spd += dp*(ci_spd/2.002);
                            cubes[i1].rot_spd /= 1.1;
                            cubes[i2].spd -= dp*(ci_spd/2.002);
                            cubes[i2].rot_spd /= 1.1;
                        }
                        if dc.magnitude2()<3. && dc.magnitude2() != 0. {
                            let penetration = 1.732 - dc.magnitude();
                            cubes[i1].p_accel -= dp*penetration*100.;
                            cubes[i1].p_time = -penetration/ci_spd;
                            cubes[i2].p_accel += dp*penetration*100.;
                            cubes[i2].p_time = -penetration/ci_spd;
                        }
                    }
                },
            };
        });

        let (lx, ly) = f64::sin_cos(self.tick*0.05);
        let (lx, ly) = (lx as f32, ly as f32);
        let light_pos = v3(lx*50., ly*50., 2.);//v3(0., 0., -100.);
        State {
            tick: self.tick + dt as f64,
            cubes: cubes,
            light_pos: light_pos,
        }
    }
}

use std::thread;
use std::sync::Arc;
use std::sync::mpsc::{channel, Receiver, Sender};

fn state_update_agent(thread_cnt: u32, rx: Receiver<(Arc<State>, f32, Point3<f32>)>, tx: Sender<Arc<State>>) {
    loop {
        if let Ok((state_ref, dt, camera)) = rx.recv() {
            if let Err(_) = tx.send(Arc::new(state_ref.update(dt, thread_cnt, camera))) {
                return ();
            }
        } else {
            return ();
        }
    }
}

pub struct StateUpdateAgent {
    tx: Sender<(Arc<State>, f32, Point3<f32>)>,
    rx: Receiver<Arc<State>>,
}

impl StateUpdateAgent {
    pub fn new(thread_cnt: u32) -> Self {
        let (otx, orx) = channel();
        let (itx, irx) = channel();
        thread::spawn(move || state_update_agent(thread_cnt, orx, itx));
        StateUpdateAgent {
            tx: otx,
            rx: irx,
        }
    }

    pub fn start_update(&self, state: Arc<State>, dt: f32, camera: Point3<f32>) -> UpdateResult {
        self.tx.send((state, dt, camera)).unwrap();
        UpdateResult{
            agent: self,
            result_aquired: false,
        }
    }
}

pub struct UpdateResult<'a> {
    agent: &'a StateUpdateAgent,
    result_aquired: bool,
}

impl<'a> UpdateResult<'a> {
    pub fn get(&mut self) -> Arc<State> {
        self.result_aquired = true;
        self.agent.rx.recv().unwrap()
    }
}

impl<'a> Drop for UpdateResult<'a> {
    fn drop(&mut self) {
        if !self.result_aquired {
            error!("Update result is not retrieved before drop");
        }
    }
}
