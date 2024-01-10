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

import org.flowable.common.engine.impl.AbstractEngineConfiguration;
import org.flowable.common.engine.impl.interceptor.EngineConfigurationConstants;
import org.flowable.idm.api.Group;
import org.flowable.idm.api.Privilege;
import org.flowable.idm.api.User;
import org.flowable.idm.engine.IdmEngineConfiguration;
import org.flowable.idm.engine.impl.IdmIdentityServiceImpl;
import org.flowable.idm.engine.impl.persistence.entity.GroupEntityImpl;
import org.flowable.idm.engine.impl.persistence.entity.PrivilegeEntityImpl;
import org.flowable.idm.engine.impl.persistence.entity.UserEntityImpl;
import org.ow2.petals.flowable.identity.AbstractProcessEngineConfigurator;
import org.ow2.petals.flowable.identity.SeFlowableIdmEngineConfigurator;
import org.ow2.petals.flowable.identity.exception.IdentityServiceInitException;
import org.ow2.petals.flowable.identity.exception.IdentityServiceResourceNotFoundException;

/**
 * A {@link SeFlowableIdmEngineConfigurator} that integrates an identity management engine based on files.
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
     * The resource used as default configuration file of the identity service, it contains the priviliege list
     */
    private static final String DEFAULT_CFG_RESOURCE_PRIVILEGES = "/file-idm-configurator-privileges.properties";

    /**
     * Property name of the identity service containing declaration of users
     */
    public static final String PROP_USERS_FILE_NAME = "users-file";

    /**
     * Property name of the identity service containing declaration of groups
     */
    public static final String PROP_GROUPS_FILE_NAME = "groups-file";

    /**
     * Property name of the identity service containing declaration of privileges
     */
    public static final String PROP_PRIVILEGES_FILE_NAME = "privileges-file";

    /**
     * Users list into a map: key=user-id, value=user-password
     */
    private final Map<String, User> users = new ConcurrentHashMap<>();

    /**
     * Groups list into a map: key=group-id, value=list of user-id
     */
    private final Map<String, List<String>> groups = new ConcurrentHashMap<>();

    /**
     * Privileges list into a map: key=privilege-id, value=list of user-id
     */
    private final Map<String, List<String>> privileges = new ConcurrentHashMap<>();

    /**
     * Groups by user into a map: key=user-id, value=list of group-id
     */
    private final Map<String, List<Group>> groupsByUser = new ConcurrentHashMap<>();

    /**
     * Groups by privilege into a map: key=user-id, value=list of privilege
     */
    private final Map<String, List<Privilege>> privilegesByUser = new ConcurrentHashMap<>();

    @Override
    public void beforeInit(final AbstractEngineConfiguration engineConfiguration) {
        // Nothing to do
    }

    @Override
    public void configure(final AbstractEngineConfiguration processEngineConfiguration) {

        this.idmEngineConfiguration = new FileIdmEngineConfiguration();

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

            // Compute the map "Privileges by user"
            for (final String user : this.users.keySet()) {
                final List<Privilege> privilegesList = new ArrayList<>();
                this.privilegesByUser.put(user, privilegesList);
                for (final Entry<String, List<String>> privilegeEntry : this.privileges.entrySet()) {
                    if (privilegeEntry.getValue().contains(user)) {
                        final PrivilegeEntityImpl privilege = new PrivilegeEntityImpl();
                        privilege.setId(privilegeEntry.getKey());
                        privilege.setName(privilegeEntry.getKey());
                        privilegesList.add(privilege);
                    }
                }
            }

            super.configure(processEngineConfiguration);

            final IdmIdentityServiceImpl idmIdentityServiceImpl = new FileIdentityServiceImpl(this.users, this.groups,
                    this.groupsByUser, this.privilegesByUser, this.idmEngineConfiguration);
            getIdmEngineConfiguration(processEngineConfiguration)
                    .setIdmIdentityService(idmIdentityServiceImpl);

        } catch (final IdentityServiceInitException e) {
            this.logger.log(Level.WARNING, "An error occurs loading files of the file-based IDM engine.", e);
        }
    }

    protected static IdmEngineConfiguration getIdmEngineConfiguration(
            final AbstractEngineConfiguration engineConfiguration) {
        return (IdmEngineConfiguration) engineConfiguration.getEngineConfigurations()
                .get(EngineConfigurationConstants.KEY_IDM_ENGINE_CONFIG);
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

                final String privilegesFileName = props.getProperty(PROP_PRIVILEGES_FILE_NAME);
                if (privilegesFileName == null || privilegesFileName.trim().isEmpty()) {
                    throw new IdentityServiceInitException(
                            "The file containing the declarations of privileges is not defined in the identity service configuration file.");
                }
                this.readPrivilegesFile(privilegesFileName);
            } else {
                this.readUsersResource(DEFAULT_CFG_RESOURCE_USERS);
                this.readGroupsResource(DEFAULT_CFG_RESOURCE_GROUPS);
                this.readPrivilegesResource(DEFAULT_CFG_RESOURCE_PRIVILEGES);
            }
        } catch (final IOException e) {
            throw new IdentityServiceInitException(e);
        }
    }

    private static Properties loadFileAsProperties(final File fileToLoad) throws IOException {

        final Properties props = new Properties();
        try (final Reader fileReader = new FileReader(fileToLoad)) {
            props.load(fileReader);
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
            } catch (final @SuppressWarnings("squid:S1166") IdentityServiceResourceNotFoundException e) {
                throw new IdentityServiceInitException(
                        "The file declaring users (" + usersFileName + ") does not exist.");
            }
        } else if (!tmpFile.isFile()) {
            throw new IdentityServiceInitException("The file declaring users (" + usersFileName + ") is not a file.");
        } else {
            try (final InputStream usersFileInputStream = new FileInputStream(tmpFile)) {
                this.readUsers(usersFileInputStream);
            } catch (final FileNotFoundException e) {
                throw new IdentityServiceInitException(e);
            } catch (final IOException e) {
                // Exception occurring on automatic close() invocation.
                throw new IdentityServiceInitException(
                        String.format("An error occurs closing file '%s'. Error skipped.", tmpFile.getAbsolutePath()),
                        e);
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
            this.readGroupsResource(groupsFileName);
        } else if (!tmpFile.exists()) {
            try {
                this.readGroupsResource(groupsFileName);
            } catch (final @SuppressWarnings("squid:S1166") IdentityServiceResourceNotFoundException e) {
                throw new IdentityServiceInitException(
                        "The file declaring groups (" + groupsFileName + ") does not exist.");
            }
        } else if (!tmpFile.isFile()) {
            throw new IdentityServiceInitException("The file declaring groups (" + groupsFileName + ") is not a file.");
        } else {
            try (final InputStream groupsFileInputStream = new FileInputStream(tmpFile)) {
                this.readGroups(groupsFileInputStream);
            } catch (final FileNotFoundException e) {
                throw new IdentityServiceInitException(e);
            } catch (final IOException e) {
                // Exception occurring on automatic close() invocation.
                throw new IdentityServiceInitException(String
                        .format("An error occurs closing resource '%s'. Error skipped.", tmpFile.getAbsolutePath()), e);
            }
        }
    }

    /**
     * Read a file containing the privilege declarations. If the file is not an absolute file, it is read a resource.
     * 
     * @param privilegesFileName
     *            The file name or resource name
     * @throws IdentityServiceInitException
     *             An error occurs reading the file.
     */
    private void readPrivilegesFile(final String privilegesFileName) throws IdentityServiceInitException {

        assert privilegesFileName != null;

        final File tmpFile = new File(privilegesFileName.trim());
        if (!tmpFile.isAbsolute()) {
            this.readPrivilegesResource(privilegesFileName);
        } else if (!tmpFile.exists()) {
            try {
                this.readPrivilegesResource(privilegesFileName);
            } catch (final IdentityServiceResourceNotFoundException e) {
                throw new IdentityServiceInitException(
                        "The file declaring privileges (" + privilegesFileName + ") does not exist.");
            }
        } else if (!tmpFile.isFile()) {
            throw new IdentityServiceInitException(
                    "The file declaring privileges (" + privilegesFileName + ") is not a file.");
        } else {
            try (final InputStream privilegesFileInputStream = new FileInputStream(tmpFile)) {
                this.readPrivileges(privilegesFileInputStream);
            } catch (final FileNotFoundException e) {
                throw new IdentityServiceInitException(e);
            } catch (final IOException e) {
                throw new IdentityServiceInitException(String
                        .format("An error occurs closing resource '%s'. Error skipped.", tmpFile.getAbsolutePath()), e);
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

        try (final InputStream usersInputStream = this.getClass().getResourceAsStream(usersResourceName)) {
            if (usersInputStream != null) {
                this.readUsers(usersInputStream);
            } else {
                throw new IdentityServiceResourceNotFoundException(usersResourceName);
            }
        } catch (final IOException e) {
            // Exception occurring on automatic close() invocation.
            throw new IdentityServiceInitException(
                    String.format("An error occurs closing resource '%s'. Error skipped.", usersResourceName), e);
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

        try (final InputStream groupsInputStream = this.getClass().getResourceAsStream(groupsResourceName)) {
            assert groupsInputStream != null : "Resource [" + groupsResourceName
                    + "] containing group declaration not found";

            this.readGroups(groupsInputStream);

        } catch (final IOException e) {
            throw new IdentityServiceInitException(
                    String.format("An error occurs closing resource '%s'. Error skipped.", groupsResourceName), e);
        }
    }

    /**
     * Read a resource containing the privileges declarations
     * 
     * @param privilegesResourceName
     *            The resource name
     * @throws IdentityServiceInit
     *             An error occurs reading the resource.
     */
    private void readPrivilegesResource(final String privilegesResourceName) throws IdentityServiceInitException {

        assert privilegesResourceName != null;

        final InputStream privilegesInputStream = this.getClass().getResourceAsStream(privilegesResourceName);
        assert privilegesInputStream != null : "Resource [" + privilegesResourceName
                + "] containing privilege declarations not found";
        try {
            this.readPrivileges(privilegesInputStream);
        } finally {
            try {
                privilegesInputStream.close();
            } catch (final IOException e) {
                throw new IdentityServiceInitException(
                        String.format("An error occurs closing resource '%s'. Error skipped.", privilegesResourceName),
                        e);
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
            final List<String> userList = this.groups.computeIfAbsent(groupId, k -> new ArrayList<>());
            final StringTokenizer tokenizer = new StringTokenizer(usersOfGroup);
            while (tokenizer.hasMoreTokens()) {
                userList.add(tokenizer.nextToken());
            }
        }
    }

    private void readPrivileges(final InputStream privilegesInputStream) throws IdentityServiceInitException {
        final Properties privilegesProps = new Properties();
        try {
            privilegesProps.load(privilegesInputStream);
        } catch (final IOException e) {
            throw new IdentityServiceInitException(e);
        }

        for (final Entry<Object, Object> entry : privilegesProps.entrySet()) {
            final String privilegeId = (String) entry.getKey();
            final String usersOrGroupsOfPrivilege = (String) entry.getValue();
            final List<String> privilegeList = this.privileges.computeIfAbsent(privilegeId, k -> new ArrayList<>());
            final StringTokenizer tokenizer = new StringTokenizer(usersOrGroupsOfPrivilege);
            while (tokenizer.hasMoreTokens()) {
                final String item = tokenizer.nextToken();
                final List<String> groupMembers = this.groups.get(item);
                if (groupMembers == null) {
                    privilegeList.add(item);
                } else {
                    privilegeList.addAll(groupMembers);
                }
            }
        }
    }
}