module Ghost

    type Post = 
        {
            id : string
            uuid : string
            title : string
            slug : string
            markdown : string
            mobiledoc : string
            html : string
            image : string
            featured : string
            page : int
            status : string
            language : string
            visibility : string
            meta_title : obj
            meta_description : obj
            author_id : string
            created_at : string
            created_by : string
            updated_at : string
            updated_by : string
            published_at : string
            published_by : obj
            amp : obj
        }

    type Meta =
        {
            exported_on : int64
            version : string
        }

    type Permission = 
        {
            id : string
            uuid : string
            name : string
            object_type : string
            action_type : string
            object_id : string
            created_at : string
            created_by : string
            updated_at : string
            updated_by : string
        }

    type PermissionsRoles = 
        {
            id : string
            role_id : string
            permission_id : string
        }

    type PostsTags =
        {
            id : string
            post_id : string
            tag_id : string
            sort_order : string
        }

    type Role = 
        {
            id : string
            uuid : string
            name : string
            description : string
            created_at : string
            created_by : string
            updated_at : string
            updated_by : string
        }

    type RolesUsers = 
        {
            id : string
            role_id : string
            user_id : string
        }

    type Settings = 
        {
            id : string
            uuid : string
            key : string
            value : string
            ``type`` : string
            created_at : string
            created_by : string
            updated_at : string
            updated_by : string
        }

    type Tag = 
        {
            id : string
            uuid : string
            name : string
            slug : string
            description : string
            image : string
            parent_id : string
            visibility : string
            meta_title : obj
            meta_description : obj
            created_at : string
            created_by : string
            updated_at : string
            updated_by : string
        }

    type User = 
        {
            id : string
            uuid : string
            name : string
            slug : string
            password : string
            email : string
            image : string
            cover : string
            bio : string
            website : obj
            location : obj
            facebook : obj
            twitter : obj
            accessibility : obj
            status : string
            language : string
            visibility : string
            meta_title : obj
            meta_description : obj
            tour : obj
            last_login : string
            created_at : string
            created_by : string
            updated_at : string
            updated_by : string
        }
        
    type Data =
        {
            posts : Post[]
            users : User[]
            roles : Role[]
            roles_users : RolesUsers[]
            permissions : Permission[]
            permissions_users : obj[]
            permissions_roles : PermissionsRoles[]
            permissions_apps : obj[]
            settings : Settings[]
            tags : Tag[]
            posts_tags : PostsTags[]
            apps : obj[]
            app_settings : obj[]
            app_fields : obj[]
            subscribers : obj[]
        }
       
    type Db =
        {
            meta : Meta
            data : Data
        }

    type GhostExport =
        {
            db : Db[]
        }