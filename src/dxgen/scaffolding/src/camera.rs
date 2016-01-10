use cgmath::*;
use utils::*;

#[derive(Clone)] // no Debug for Basis3
pub struct Camera {
    pub eye: Point3<f32>,
    pub fwd: Vector3<f32>,
    pub up: Vector3<f32>,
    pub right: Vector3<f32>,
    pub rot: Basis3<f32>,
    pub hfov_deg: f32,
    pub aspect: f32,
    pub near: f32,
    pub far: f32,
}

impl Camera {
    pub fn new() -> Camera {
        Camera {
            eye: p3(0., 0., 0.),
            fwd: v3(0., 0., -1.),
            up: v3(0., 1., 0.),
            right: v3(1., 0., 0.),
            rot: <Basis3<f32> as Rotation<_>>::one(),
            hfov_deg: 60.,
            aspect: 1.,
            near: 0.1,
            far: 1000.,
        }
    }

    pub fn rotx(&mut self, ang: f32) {
        let rotm = Basis3::from_axis_angle(self.up, Angle::from(deg(ang)));
        self.rot = rotm.concat(&self.rot);
        self.update_axes(); //TODO: optimize, when it becomes not early optimization.
    }

    pub fn roty(&mut self, ang: f32) {
        let rotm = Basis3::from_axis_angle(self.right, Angle::from(deg(ang)));
        self.rot = rotm.concat(&self.rot);
        self.update_axes();
    }

    pub fn rotz(&mut self, ang: f32) {
        // vv Isn't it supposed to be generic over angle type?
        let rotm = Basis3::from_axis_angle(self.fwd, Angle::from(deg(ang)));
        self.rot = rotm.concat(&self.rot);
        self.update_axes();
    }

    fn update_axes(&mut self) {
        self.fwd = self.rot.rotate_vector(v3(0., 0., -1.));
        self.right = self.rot.rotate_vector(v3(1., 0., 0.));
        self.up = self.rot.rotate_vector(v3(0., 1., 0.));
    }

    pub fn go(&mut self, fwd: f32, right: f32, up: f32) {
        self.eye = self.eye + self.fwd * fwd + self.right * right + self.up * up;
    }

    pub fn view_matrix(&self) -> Matrix4<f32> {
        let mut vm = Matrix4::look_at(self.eye, self.eye + self.fwd, self.up);
        // lhs_to_rhs(&mut vm);
        vm
    }

    pub fn projection_matrix(&self) -> Matrix4<f32> {
        perspective(deg(self.hfov_deg), self.aspect, self.near, self.far)
    }
}
