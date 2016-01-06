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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.concurrent.ConcurrentHashMap;

import org.activiti.engine.identity.Group;
import org.activiti.engine.identity.User;
import org.activiti.engine.impl.interceptor.SessionFactory;
import org.activiti.engine.impl.persistence.entity.GroupEntity;
import org.activiti.engine.impl.persistence.entity.UserEntity;
import org.ow2.petals.activitibpmn.identity.IdentityService;
import org.ow2.petals.activitibpmn.identity.exception.IdentityServiceInitException;
import org.ow2.petals.activitibpmn.identity.exception.IdentityServiceResourceNotFoundException;

public class FileIdentityService implements IdentityService {

    /**
     * The resource used as default configuration file of the identity service, it contains the user list
     */
    private static final String DEFAULT_CFG_RESOURCE_USERS = "/file-identity-service-users.properties";

    /**
     * The resource used as default configuration file of the identity service, it contains the group list
     */
    private static final String DEFAULT_CFG_RESOURCE_GROUPS = "/file-identity-service-groups.properties";

    /**
     * Property name of the identity service containing declaration of users
     */
    public static final String PROP_USERS_FILE_NAME = "users-file";

    /**
     * Property name of the identity service containing declaration of groups
     */
    public static final String PROP_GROUPS_FILE_NAME = "groups-file";

    /**
     * Users list into a map: key=user-id, value=user-password
     */
    private final Map<String, User> users = new ConcurrentHashMap<String, User>();

    /**
     * Groups list into a map: key=group-id, value=list of user-id
     */
    private final Map<String, List<String>> groups = new ConcurrentHashMap<String, List<String>>();

    /**
     * Groups by user into a map: key=user-id, value=list of group-id
     */
    private final Map<String, List<Group>> groupsByUser = new ConcurrentHashMap<String, List<Group>>();

    @Override
    public SessionFactory getUserEntityManagerFactory() {
        return new FileUserEntityManagerFactory(this.users, this.groupsByUser);
    }

    @Override
    public SessionFactory getGroupEntityManagerFactory() {
        return new FileGroupEntityManagerFactory(this.groupsByUser);
    }

    @Override
    public SessionFactory getMembershipEntityManagerFactory() {
        return new FileMembershipEntityManagerFactory();
    }

    @Override
    public void init(final File configurationFile) throws IdentityServiceInitException {

        assert configurationFile == null || (configurationFile != null && configurationFile.exists()) : "The configuration file does not exist";
        assert configurationFile == null || (configurationFile != null && configurationFile.isFile()) : "The configuration file is not a valid file";

        try {
            if (configurationFile != null) {
                final Properties props = new Properties();
                final Reader cfgFileReader = new FileReader(configurationFile);
                try {
                    props.load(cfgFileReader);
                } finally {
                    try {
                        cfgFileReader.close();
                    } catch (final IOException e) {
                        // NOP: We discard exception on close
                    }
                }

                final String usersFileName = props.getProperty(PROP_USERS_FILE_NAME);
                if (usersFileName == null || usersFileName.trim().isEmpty()) {
                    throw new IdentityServiceInitException(
                            "The file containing the declarations of users is not defined in the identity service configuration file.");
                }
                this.readUsersFile(usersFileName);

                final String groupsFileName = props.getProperty(PROP_GROUPS_FILE_NAME);
                if (groupsFileName == null || groupsFileName.trim().isEmpty()) {
                    throw new IdentityServiceInitException(
                            "The file containing the declarations of groups is not defined in the identity service configuration file.");
                }
                this.readGroupsFile(groupsFileName);
            } else {
                this.readUsersResource(DEFAULT_CFG_RESOURCE_USERS);
                this.readGroupsResource(DEFAULT_CFG_RESOURCE_GROUPS);
            }
        } catch (final IOException e) {
            throw new IdentityServiceInitException(e);
        }

        // Compute the map "Groups by user"
        for (final String user : this.users.keySet()) {
            final List<Group> groupsList = new ArrayList<Group>();
            this.groupsByUser.put(user, groupsList);
            for (final Entry<String, List<String>> groupEntry : this.groups.entrySet()) {
                if (groupEntry.getValue().contains(user)) {
                    final Group group = new GroupEntity(groupEntry.getKey());
                    groupsList.add(group);
                }
            }
        }
    }

    /**
     * Read a file containing the user declarations. If the file is not an absolute file, it is read a resource.
     * 
     * @param usersFileName
     *            The file name or resource name
     * @throws IdentityServiceInitException
     *             An error occurs reading the file.
     */
    private void readUsersFile(final String usersFileName) throws IdentityServiceInitException {

        assert usersFileName != null;

        final File tmpFile = new File(usersFileName.trim());
        if (!tmpFile.isAbsolute()) {
            this.readUsersResource(usersFileName);
        } else if (!tmpFile.exists()) {
            try {
                this.readUsersResource(usersFileName);
            } catch (final IdentityServiceResourceNotFoundException e) {
                throw new IdentityServiceInitException("The file declaring users (" + usersFileName
                        + ") does not exist.");
            }
        } else if (!tmpFile.isFile()) {
            throw new IdentityServiceInitException("The file declaring users (" + usersFileName + ") is not a file.");
        } else {
            try {
                final InputStream usersFileInputStream = new FileInputStream(tmpFile);
                assert usersFileInputStream != null;
                try {
                    this.readUsers(usersFileInputStream);
                } finally {
                    try {
                        usersFileInputStream.close();
                    } catch (final IOException e) {
                        // NOP: We discard exception on close
                    }
                }
            } catch (final FileNotFoundException e) {
                throw new IdentityServiceInitException(e);
            }
        }
    }

    /**
     * Read a resource containing the user declarations
     * 
     * @param usersResourceName
     *            The resource name
     * @throws IdentityServiceResourceNotFoundException
     *             Resource not found
     * @throws IdentityServiceInit
     *             An error occurs reading the resource.
     */
    private void readUsersResource(final String usersResourceName) throws IdentityServiceInitException {

        assert usersResourceName != null;

        final InputStream usersInputStream = FileIdentityService.class.getResourceAsStream(usersResourceName);
        if (usersInputStream != null) {
            try {
                this.readUsers(usersInputStream);
            } finally {
                try {
                    usersInputStream.close();
                } catch (final IOException e) {
                    // NOP: We discard exception on close
                }
            }
        } else {
            throw new IdentityServiceResourceNotFoundException(usersResourceName);
        }
    }

    private void readUsers(final InputStream usersInputStream) throws IdentityServiceInitException {
        final Properties usersProps = new Properties();
        try {
            usersProps.load(usersInputStream);
        } catch (final IOException e) {
            throw new IdentityServiceInitException(e);
        }

        for (final Entry<Object, Object> entry : usersProps.entrySet()) {
            final User user = new UserEntity((String) entry.getKey());
            user.setPassword((String) entry.getValue());
            this.users.put((String) entry.getKey(), user);
        }
    }

    /**
     * Read a file containing the group declarations. If the file is not an absolute file, it is read a resource.
     * 
     * @param groupsFileName
     *            The file name or resource name
     * @throws IdentityServiceInitException
     *             An error occurs reading the file.
     */
    private void readGroupsFile(final String groupsFileName) throws IdentityServiceInitException {

        assert groupsFileName != null;

        final File tmpFile = new File(groupsFileName.trim());
        if (!tmpFile.isAbsolute()) {
            readGroupsResource(groupsFileName);
        } else if (!tmpFile.exists()) {
            try {
                this.readGroupsResource(groupsFileName);
            } catch (final IdentityServiceResourceNotFoundException e) {
                throw new IdentityServiceInitException("The file declaring groups (" + groupsFileName
                        + ") does not exist.");
            }
        } else if (!tmpFile.isFile()) {
            throw new IdentityServiceInitException("The file declaring groups (" + groupsFileName + ") is not a file.");
        } else {
            try {
                final InputStream groupsFileInputStream = new FileInputStream(tmpFile);
                assert groupsFileInputStream != null;
                try {
                    this.readGroups(groupsFileInputStream);
                } finally {
                    try {
                        groupsFileInputStream.close();
                    } catch (final IOException e) {
                        // NOP: We discard exception on close
                    }
                }
            } catch (final FileNotFoundException e) {
                throw new IdentityServiceInitException(e);
            }
        }
    }

    /**
     * Read a resource containing the group declarations
     * 
     * @param groupsResourceName
     *            The resource name
     * @throws IdentityServiceInit
     *             An error occurs reading the resource.
     */
    private void readGroupsResource(final String groupsResourceName) throws IdentityServiceInitException {

        assert groupsResourceName != null;

        final InputStream groupsInputStream = FileIdentityService.class.getResourceAsStream(groupsResourceName);
        assert groupsInputStream != null : "Resource [" + groupsResourceName
                + "] containing group declaration not found";
        try {
            this.readGroups(groupsInputStream);
        } finally {
            try {
                groupsInputStream.close();
            } catch (final IOException e) {
                // NOP: We discard exception on close
            }
        }
    }

    private void readGroups(final InputStream groupsInputStream) throws IdentityServiceInitException {
        final Properties groupsProps = new Properties();
        try {
            groupsProps.load(groupsInputStream);
        } catch (final IOException e) {
            throw new IdentityServiceInitException(e);
        }

        for (final Entry<Object, Object> entry : groupsProps.entrySet()) {
            final String groupId = (String)entry.getKey();
            final String users = (String)entry.getValue();
            List<String> userList = this.groups.get(groupId);
            if (userList == null) {
                userList = new ArrayList<String>();
                this.groups.put(groupId, userList);
            }
            final StringTokenizer tokenizer = new StringTokenizer(users);
            while (tokenizer.hasMoreTokens()) {
                userList.add(tokenizer.nextToken());
            }
        }
    }

}
