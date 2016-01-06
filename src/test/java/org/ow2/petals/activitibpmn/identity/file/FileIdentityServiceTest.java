/**
 * Copyright (c) 2015-2016 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.identity.file;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.Properties;

import org.activiti.engine.IdentityService;
import org.activiti.engine.identity.Group;
import org.activiti.engine.identity.User;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.ow2.petals.activitibpmn.identity.exception.IdentityServiceInitException;
import org.ow2.petals.activitibpmn.junit.ActivitiClient;

public class FileIdentityServiceTest {

    @Rule
    public final TemporaryFolder tempFolder = new TemporaryFolder();

    @Rule
    public final ActivitiClient activitiClient = new ActivitiClient();
    /**
     * Check the loading of the default configuration embedded into the file-based identity service.
     */
    @Test
    public void embeddedConfiguration() throws IdentityServiceInitException {
        
        final FileIdentityService fis = new FileIdentityService();
        fis.init(null);
        
        final FileGroupManager fileGrpMngr = (FileGroupManager) fis.getGroupEntityManagerFactory().openSession();

        final List<Group> kermitGroups = fileGrpMngr.findGroupsByUser("kermit");
        assertEquals(6, kermitGroups.size());
        
        final FileUserManager fileUserMngr = (FileUserManager) fis.getUserEntityManagerFactory().openSession();
        final User kermitUser = fileUserMngr.findUserById("kermit");
        assertEquals("kermit", kermitUser.getPassword());

        final List<Group> gonzoGroups = fileUserMngr.findGroupsByUser("gonzo");
        assertEquals(4, gonzoGroups.size());
        
        assertTrue(fileUserMngr.checkPassword("fozzie", "fozzie"));
        assertFalse(fileUserMngr.checkPassword("kermit", "fozzie"));

        final IdentityService identityService = this.activitiClient.getIdentityService();
        final List<User> membersOfMngt = identityService.createUserQuery().memberOfGroup("management").list();
        assertEquals(2, membersOfMngt.size());
        assertFalse(membersOfMngt.get(0).getId().equals(membersOfMngt.get(1).getId()));
        assertTrue(membersOfMngt.get(0).getId().equals("kermit") || membersOfMngt.get(0).getId().equals("gonzo"));
        assertTrue(membersOfMngt.get(1).getId().equals("kermit") || membersOfMngt.get(1).getId().equals("gonzo"));

        final List<User> membersOfUnexisting = identityService.createUserQuery().memberOfGroup("unexisting-grp").list();
        assertEquals(0, membersOfUnexisting.size());

        final List<Group> groupOfFozzie = identityService.createGroupQuery().groupMember("fozzie").list();
        assertEquals(2, groupOfFozzie.size());
        assertFalse(groupOfFozzie.get(0).getId().equals(groupOfFozzie.get(1).getId()));
        assertTrue(groupOfFozzie.get(0).getId().equals("accountancy") || groupOfFozzie.get(0).getId().equals("user"));
        assertTrue(groupOfFozzie.get(1).getId().equals("accountancy") || groupOfFozzie.get(1).getId().equals("user"));

        final List<Group> groupOfUnknownUser = identityService.createGroupQuery().groupMember("unknown-user").list();
        assertEquals(0, groupOfUnknownUser.size());

    }

    /**
     * Check the loading of given configuration.
     */
    @Test
    public void givenConfiguration() throws IdentityServiceInitException, IOException {

        final Properties usersConfig = new Properties();
        final String user1 = "user-1";
        final String user2 = "user-2";
        final String user2Pwd = "user2";
        usersConfig.setProperty(user1, "user1");
        usersConfig.setProperty(user2, user2Pwd);
        final File usersConfigFile = this.tempFolder.newFile("usersConfigFile.properties");
        final OutputStream osUsersConfigFile = new FileOutputStream(usersConfigFile);
        try {
            usersConfig.store(osUsersConfigFile, "");
        } finally {
            osUsersConfigFile.close();
        }

        final Properties groupsConfig = new Properties();
        final String group1 = "group-1";
        final String group2 = "group-2";
        groupsConfig.setProperty(group1, user1);
        groupsConfig.setProperty(group2, user1 + " " + user2);
        final File groupsConfigFile = this.tempFolder.newFile("groupsConfigFile.properties");
        final OutputStream osGroupsConfigFile = new FileOutputStream(groupsConfigFile);
        try {
            groupsConfig.store(osGroupsConfigFile, "");
        } finally {
            osGroupsConfigFile.close();
        }

        final Properties baseConfig = new Properties();
        baseConfig.setProperty(FileIdentityService.PROP_USERS_FILE_NAME, usersConfigFile.getAbsolutePath());
        baseConfig.setProperty(FileIdentityService.PROP_GROUPS_FILE_NAME, groupsConfigFile.getAbsolutePath());
        final File baseConfigFile = this.tempFolder.newFile("baseConfigFile.properties");
        final OutputStream osBaseConfigFile = new FileOutputStream(baseConfigFile);
        try {
            baseConfig.store(osBaseConfigFile, "");
        } finally {
            osBaseConfigFile.close();
        }

        final FileIdentityService fis = new FileIdentityService();
        fis.init(baseConfigFile);

        final FileGroupManager fileGrpMngr = (FileGroupManager) fis.getGroupEntityManagerFactory().openSession();

        final List<Group> user1Groups = fileGrpMngr.findGroupsByUser(user1);
        assertEquals(2, user1Groups.size());

        final FileUserManager fileUserMngr = (FileUserManager) fis.getUserEntityManagerFactory().openSession();
        final User user2User = fileUserMngr.findUserById(user2);
        assertEquals(user2Pwd, user2User.getPassword());

        assertTrue(fileUserMngr.checkPassword(user2, user2Pwd));
        assertFalse(fileUserMngr.checkPassword(user1, "invalid-pwd"));

    }

}
