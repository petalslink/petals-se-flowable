/**
 * Copyright (c) 2015-2024 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.identity.file;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.flowable.engine.impl.util.EngineServiceUtil;
import org.flowable.idm.api.Group;
import org.flowable.idm.api.IdmIdentityService;
import org.flowable.idm.api.Privilege;
import org.flowable.idm.api.User;
import org.junit.jupiter.api.Test;
import org.ow2.petals.flowable.junit.extensions.FlowableClientExtension;
import org.ow2.petals.flowable.junit.extensions.api.FlowableClient;

public class FileIdentityServiceImplTest {

    /**
     * Check the loading of the default configuration embedded into the file-based IDM engine.
     */
    @Test
    public void embeddedConfiguration(final @FlowableClientExtension FlowableClient flowableClient) throws Exception {

        final IdmIdentityService idmIdentityService = EngineServiceUtil
                .getIdmIdentityService(flowableClient.getProcessEngine().getProcessEngineConfiguration());

        final List<Group> kermitGroups = idmIdentityService.createGroupQuery().groupMember("kermit").list();
        assertEquals(6, kermitGroups.size());

        final User kermitUser = idmIdentityService.createUserQuery().userId("kermit").singleResult();
        assertEquals("kermit", kermitUser.getPassword());

        final List<Group> gonzoGroups = idmIdentityService.createGroupQuery().groupMember("gonzo").list();
        assertEquals(4, gonzoGroups.size());

        assertTrue(idmIdentityService.checkPassword("fozzie", "fozzie"));
        assertFalse(idmIdentityService.checkPassword("kermit", "fozzie"));

        final List<User> membersOfMngt = idmIdentityService.createUserQuery().memberOfGroup("management").list();
        assertEquals(2, membersOfMngt.size());
        assertFalse(membersOfMngt.get(0).getId().equals(membersOfMngt.get(1).getId()));
        assertTrue(membersOfMngt.get(0).getId().equals("kermit") || membersOfMngt.get(0).getId().equals("gonzo"));
        assertTrue(membersOfMngt.get(1).getId().equals("kermit") || membersOfMngt.get(1).getId().equals("gonzo"));

        final List<User> membersOfUnexisting = idmIdentityService.createUserQuery().memberOfGroup("unexisting-grp")
                .list();
        assertEquals(0, membersOfUnexisting.size());

        final List<Group> groupOfFozzie = idmIdentityService.createGroupQuery().groupMember("fozzie").list();
        assertEquals(2, groupOfFozzie.size());
        assertFalse(groupOfFozzie.get(0).getId().equals(groupOfFozzie.get(1).getId()));
        assertTrue(groupOfFozzie.get(0).getId().equals("accountancy") || groupOfFozzie.get(0).getId().equals("user"));
        assertTrue(groupOfFozzie.get(1).getId().equals("accountancy") || groupOfFozzie.get(1).getId().equals("user"));

        final List<Group> groupOfUnknownUser = idmIdentityService.createGroupQuery().groupMember("unknown-user").list();
        assertEquals(0, groupOfUnknownUser.size());

        // Privileges of a direct user
        final List<Privilege> kermitPrivileges = idmIdentityService.createPrivilegeQuery().userId("kermit").list();
        assertEquals(1, kermitPrivileges.size());
        assertEquals("rest-access-api", kermitPrivileges.get(0).getName());

        // Privileges of an indirect user (definition through group)
        final List<Privilege> byGroupPrivileges = idmIdentityService.createPrivilegeQuery().userId("rest-api-user")
                .list();
        assertEquals(1, byGroupPrivileges.size());
        assertEquals("rest-access-api", byGroupPrivileges.get(0).getName());
    }

    /**
     * Check the loading of a given configuration.
     */
    @Test
    public void givenConfiguration(
            final @FlowableClientExtension(idmEngineConfiguratorCfgFile = "org/ow2/petals/flowable/identity/file/idm-engine-configurator.properties") FlowableClient flowableClient)
            throws Exception {

        // User/Password defined according to the content of the IDM engine configuration file of the Flowable client.
        final String user1 = "user-1";
        final String user2 = "user-2";
        final String user2Pwd = "user2";

        final IdmIdentityService idemIdentityService = EngineServiceUtil
                .getIdmIdentityService(flowableClient.getProcessEngine().getProcessEngineConfiguration());

        final List<Group> user1Groups = idemIdentityService.createGroupQuery().groupMember(user1).list();
        assertEquals(2, user1Groups.size());

        final User user2User = idemIdentityService.createUserQuery().userId(user2).singleResult();
        assertEquals(user2Pwd, user2User.getPassword());

        assertTrue(idemIdentityService.checkPassword(user2, user2Pwd));
        assertFalse(idemIdentityService.checkPassword(user1, "invalid-pwd"));

        final List<Privilege> user1privileges = idemIdentityService.createPrivilegeQuery().userId(user1).list();
        assertEquals(2, user1privileges.size());
    }
}
