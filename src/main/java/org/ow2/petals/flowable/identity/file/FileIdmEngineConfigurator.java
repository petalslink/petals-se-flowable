/**
 * Copyright (c) 2015-2018 Linagora
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
import java.util.logging.Level;

import org.flowable.engine.cfg.ProcessEngineConfigurator;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.flowable.idm.api.Group;
import org.flowable.idm.api.User;
import org.flowable.idm.engine.impl.persistence.entity.GroupEntityImpl;
import org.flowable.idm.engine.impl.persistence.entity.UserEntityImpl;
import org.ow2.petals.flowable.identity.AbstractProcessEngineConfigurator;
import org.ow2.petals.flowable.identity.exception.IdentityServiceInitException;
import org.ow2.petals.flowable.identity.exception.IdentityServiceResourceNotFoundException;

/**
 * A {@link ProcessEngineConfigurator} that integrates an identity management engine based on files.
 * 
 * @author Christophe DENEUX - Linagora
 */
public class FileIdmEngineConfigurator extends AbstractProcessEngineConfigurator {

    /**
     * The resource used as default configuration file of the identity service, it contains the user list
     */
    private static final String DEFAULT_CFG_RESOURCE_USERS = "/file-idm-configurator-users.properties";

    /**
     * The resource used as default configuration file of the identity service, it contains the group list
     */
    private static final String DEFAULT_CFG_RESOURCE_GROUPS = "/file-idm-configurator-groups.properties";

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
    private final Map<String, User> users = new ConcurrentHashMap<>();

    /**
     * Groups list into a map: key=group-id, value=list of user-id
     */
    private final Map<String, List<String>> groups = new ConcurrentHashMap<>();

    /**
     * Groups by user into a map: key=user-id, value=list of group-id
     */
    private final Map<String, List<Group>> groupsByUser = new ConcurrentHashMap<>();

    @Override
    public void configure(final ProcessEngineConfigurationImpl processEngineConfiguration) {

        try {
            this.loadFiles();

            // Compute the map "Groups by user"
            for (final String user : this.users.keySet()) {
                final List<Group> groupsList = new ArrayList<>();
                this.groupsByUser.put(user, groupsList);
                for (final Entry<String, List<String>> groupEntry : this.groups.entrySet()) {
                    if (groupEntry.getValue().contains(user)) {
                        final Group group = new GroupEntityImpl();
                        group.setId(groupEntry.getKey());
                        groupsList.add(group);
                    }
                }
            }

            processEngineConfiguration
                    .setIdmIdentityService(new FileIdentityServiceImpl(this.users, this.groups, this.groupsByUser));

        } catch (final IdentityServiceInitException e) {
            this.logger.log(Level.WARNING, "An error occurs loading files of the file-based IDM engine.", e);
        }
    }

    private void loadFiles() throws IdentityServiceInitException {

        assert this.configurationFile == null || (this.configurationFile != null
                && this.configurationFile.exists()) : "The configuration file does not exist";
        assert this.configurationFile == null || (this.configurationFile != null
                && this.configurationFile.isFile()) : "The configuration file is not a valid file";

        try {
            if (this.configurationFile != null) {
                final Properties props = loadFileAsProperties(this.configurationFile);

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
    }

    private static Properties loadFileAsProperties(final File fileToLoad) throws IOException {

        final Properties props = new Properties();
        final Reader fileReader = new FileReader(fileToLoad);
        try {
            props.load(fileReader);
        } finally {
            try {
                fileReader.close();
            } catch (final IOException e) {
                // NOP: We discard exception on close
            }
        }

        return props;

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
                throw new IdentityServiceInitException(
                        "The file declaring users (" + usersFileName + ") does not exist.");
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
                throw new IdentityServiceInitException(
                        "The file declaring groups (" + groupsFileName + ") does not exist.");
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
                        this.logger.log(Level.WARNING, String.format(
                                "An error occurs closing file '%s'. Error skipped.", tmpFile.getAbsolutePath()), e);
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

        final InputStream usersInputStream = this.getClass().getResourceAsStream(usersResourceName);
        if (usersInputStream != null) {
            try {
                this.readUsers(usersInputStream);
            } finally {
                try {
                    usersInputStream.close();
                } catch (final IOException e) {
                    // NOP: We discard exception on close
                    this.logger.log(Level.WARNING,
                            String.format("An error occurs closing resource '%s'. Error skipped.", usersResourceName),
                            e);
                }
            }
        } else {
            throw new IdentityServiceResourceNotFoundException(usersResourceName);
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

        final InputStream groupsInputStream = this.getClass().getResourceAsStream(groupsResourceName);
        assert groupsInputStream != null : "Resource [" + groupsResourceName
                + "] containing group declaration not found";
        try {
            this.readGroups(groupsInputStream);
        } finally {
            try {
                groupsInputStream.close();
            } catch (final IOException e) {
                // NOP: We discard exception on close
                this.logger.log(Level.WARNING,
                        String.format("An error occurs closing resource '%s'. Error skipped.", groupsResourceName), e);
            }
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
            final User user = new UserEntityImpl();
            user.setId((String) entry.getKey());
            user.setPassword((String) entry.getValue());
            this.users.put((String) entry.getKey(), user);
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
            final String groupId = (String) entry.getKey();
            final String usersOfGroup = (String) entry.getValue();
            List<String> userList = this.groups.get(groupId);
            if (userList == null) {
                userList = new ArrayList<>();
                this.groups.put(groupId, userList);
            }
            final StringTokenizer tokenizer = new StringTokenizer(usersOfGroup);
            while (tokenizer.hasMoreTokens()) {
                userList.add(tokenizer.nextToken());
            }
        }
    }

}
