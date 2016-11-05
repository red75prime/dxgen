use cgmath::{Vector3};
use utils::v3;

type Float = f32;
type V3 = Vector3<Float>;
type Id = u32;
type AABB = (V3,V3);

struct Cell {
    objs: Vec<(AABB, Id)>,
    cells: Option<Box<[[[Cell;2];2];2]>>,
}

pub struct CollisionTree {
    center: V3,
    span: Float,
    cell: Cell,
}

fn strictly_greater(a: V3, b: V3) -> bool {
    a.x > b.x && a.y > b.y && a.z > b.z
}

fn strictly_less(a: V3, b: V3) -> bool {
    a.x < b.x && a.y < b.y && a.z < b.z
}

fn is_a_inside_b(a: AABB, b: AABB) -> bool {
    strictly_greater(a.0, b.0) && strictly_less(a.1, b.1)
}

impl CollisionTree {
    pub fn new() -> CollisionTree {
        CollisionTree {
            center: v3(0.,0.,0.),
            span: 1000.,
            cell: Cell{
                objs: vec![],
                cells: None,
            },
        }
    }

    pub fn add(&mut self, id: Id, bb: AABB) -> Result<(),()> {
        let span = self.span;
        let mut cur_aabb = (self.center+v3(-span, -span, -span), self.center+v3(span, span, span)); 
        if is_a_inside_b(bb, cur_aabb) {

            Ok(())
        } else {
            // Object is not in tracking space
            Err(())
        }
    }
}

#[cfg(test)]
mod test {
    use utils::v3;
    use super::is_a_inside_b;

    #[test]
    fn test_inside1() {
        assert_eq!(is_a_inside_b((v3(0., 0., 0.,),v3(1., 1., 1.)),(v3(-1., -1., -1.),v3(2.,2.,2.))), true);
        assert_eq!(is_a_inside_b((v3(0., 0., 0.,),v3(3., 1., 1.)),(v3(-1., -1., -1.),v3(2.,2.,2.))), false);
        assert_eq!(is_a_inside_b((v3(0., 0., 0.,),v3(1., 3., 1.)),(v3(-1., -1., -1.),v3(2.,2.,2.))), false);
        assert_eq!(is_a_inside_b((v3(0., 0., 0.,),v3(1., 1., 3.)),(v3(-1., -1., -1.),v3(2.,2.,2.))), false);
        assert_eq!(is_a_inside_b((v3(-2., 0., 0.,),v3(1., 1., 1.)),(v3(-1., -1., -1.),v3(2.,2.,2.))), false);
        assert_eq!(is_a_inside_b((v3(0., -2., 0.,),v3(1., 1., 1.)),(v3(-1., -1., -1.),v3(2.,2.,2.))), false);
        assert_eq!(is_a_inside_b((v3(0., 0., -2.,),v3(1., 1., 1.)),(v3(-1., -1., -1.),v3(2.,2.,2.))), false);
    }
}
