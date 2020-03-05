/**
 * Copyright (c) 2017-2020 Linagora
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
package org.ow2.petals.flowable.identity.ldap;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.flowable.engine.common.AbstractEngineConfiguration;
import org.flowable.ldap.LDAPConfiguration;
import org.flowable.ldap.LDAPConfigurator;
import org.ow2.petals.flowable.identity.SeFlowableIdmServiceConfigurator;
import org.ow2.petals.flowable.identity.exception.IdentityServiceInitException;
import org.ow2.petals.flowable.identity.exception.IdentityServiceResourceNotFoundException;

/**
 * A {@link SeFlowableIdmServiceConfigurator} that integrates an identity management engine based on LDAP.
 * 
 * @author Christophe DENEUX - Linagora
 */
public class LdapIdmEngineConfigurator extends LDAPConfigurator implements SeFlowableIdmServiceConfigurator {

    /**
     * The resource used as default configuration file of the identity service, it contains the LDAP configuration
     */
    private static final String DEFAULT_CFG_RESOURCE = "/ldap-idm-configurator.properties";

    /**
     * Property name of the identity service containing declaration of parameter 'server'
     */
    public static final String PROP_SERVER = "server";

    /**
     * Property name of the identity service containing declaration of parameter 'port'
     */
    public static final String PROP_PORT = "port";

    /**
     * Property name of the identity service containing declaration of parameter 'securityAuthentication'
     */
    public static final String PROP_SECURITY_AUTHENTICATION = "securityAuthentication";

    /**
     * Value of {@link #PROP_SECURITY_AUTHENTICATION} for a no authentication
     */
    public static final String PROP_SECURITY_AUTHENTICATION_NONE = "none";

    /**
     * Value of {@link #PROP_SECURITY_AUTHENTICATION} for a simple authentication
     */
    public static final String PROP_SECURITY_AUTHENTICATION_SIMPLE = "simple";

    /**
     * Value of {@link #PROP_SECURITY_AUTHENTICATION} for a strong authentication
     */
    public static final String PROP_SECURITY_AUTHENTICATION_STRONG = "strong";

    /**
     * Default value of {@link #PROP_SECURITY_AUTHENTICATION}
     */
    public static final String PROP_SECURITY_AUTHENTICATION_DEFAULT = PROP_SECURITY_AUTHENTICATION_SIMPLE;

    /**
     * Property name of the identity service containing declaration of parameter 'user'
     */
    public static final String PROP_USER = "user";

    /**
     * Property name of the identity service containing declaration of parameter 'password'
     */
    public static final String PROP_PASSWORD = "password";

    /**
     * Property name of the identity service containing declaration of parameter 'searchTimeLimit'
     */
    public static final String PROP_SEARCH_TIME_LIMIT = "searchTimeLimit";

    /**
     * Property name of the identity service containing declaration of parameter 'baseDn'
     */
    public static final String PROP_BASE_DN = "baseDn";

    /**
     * Property name of the identity service containing declaration of parameter 'userBaseDn'
     */
    public static final String PROP_USER_BASE_DN = "userBaseDn";

    /**
     * Property name of the identity service containing declaration of parameter 'groupBaseDn'
     */
    public static final String PROP_GROUP_BASE_DN = "groupBaseDn";

    /**
     * Property name of the identity service containing declaration of parameter 'queryUserByUserId'
     */
    public static final String PROP_QUERY_USER_BY_USER_ID = "queryUserByUserId";

    /**
     * Property name of the identity service containing declaration of parameter 'queryUserByFullNameLike'
     */
    public static final String PROP_QUERY_USER_BY_FULLNAME_LIKE = "queryUserByFullNameLike";

    /**
     * Property name of the identity service containing declaration of parameter 'queryAllUsers'
     */
    public static final String PROP_QUERY_ALL_USERS = "queryAllUsers";

    /**
     * Property name of the identity service containing declaration of parameter 'queryGroupsForUser'
     */
    public static final String PROP_QUERY_GROUPS_FOR_USER = "queryGroupsForUser";

    /**
     * Property name of the identity service containing declaration of parameter 'queryAllGroups'
     */
    public static final String PROP_QUERY_ALL_GROUPS = "queryAllGroups";

    /**
     * Property name of the identity service containing declaration of parameter 'userIdAttribute'
     */
    public static final String PROP_ATTR_USER_ID = "userIdAttribute";

    /**
     * Property name of the identity service containing declaration of parameter 'userFirstNameAttribute'
     */
    public static final String PROP_ATTR_USER_FIRSTNAME = "userFirstNameAttribute";

    /**
     * Property name of the identity service containing declaration of parameter 'userLastNameAttribute'
     */
    public static final String PROP_ATTR_USER_LASTNAME = "userLastNameAttribute";

    /**
     * Property name of the identity service containing declaration of parameter 'userEmailAttribute'
     */
    public static final String PROP_ATTR_USER_EMAIL = "userEmailAttribute";

    /**
     * Property name of the identity service containing declaration of parameter 'groupIdAttribute'
     */
    public static final String PROP_ATTR_GROUP_ID = "groupIdAttribute";

    /**
     * Property name of the identity service containing declaration of parameter 'groupNameAttribute'
     */
    public static final String PROP_ATTR_GROUP_NAME = "groupNameAttribute";

    /**
     * The configuration file of this identity management engine.
     */
    private File configurationFile;

    private Logger logger;

    @Override
    public void configure(final AbstractEngineConfiguration processEngineConfiguration) {

        assert logger != null;
        try {
            this.ldapConfiguration = this.loadConfiguration();

            super.configure(processEngineConfiguration);
        } catch (final IdentityServiceInitException e) {
            this.logger.log(Level.WARNING, "An error occurs loading files of the LDAP-based IDM engine.", e);
        }
    }

    @Override
    public void setConfigurationFile(final File configurationFile) {
        this.configurationFile = configurationFile;
    }

    @Override
    public void setLogger(final Logger logger) {
        this.logger = logger;
    }

    private LDAPConfiguration loadConfiguration() throws IdentityServiceInitException {

        assert this.configurationFile == null || (this.configurationFile != null
                && this.configurationFile.exists()) : "The configuration file does not exist";
        assert this.configurationFile == null || (this.configurationFile != null
                && this.configurationFile.isFile()) : "The configuration file is not a valid file";

        if (this.configurationFile != null) {
            return this.readConfigurationAsFile(this.configurationFile);
        } else {
            return this.readConfigurationAsResource(DEFAULT_CFG_RESOURCE);
        }
    }

    /**
     * Read a file containing the LDAP configuration
     * 
     * @param cfgFile
     *            The file containing the LDAP configuration
     * @throws IdentityServiceResourceNotFoundException
     *             Resource not found
     * @throws IdentityServiceInit
     *             An error occurs reading the resource.
     */
    private LDAPConfiguration readConfigurationAsFile(final File cfgFile) throws IdentityServiceInitException {

        assert cfgFile != null;

        try (final InputStream cfgInputStream = new FileInputStream(cfgFile)) {
            return this.readConfiguration(cfgInputStream);
        } catch (final FileNotFoundException e) {
            throw new IdentityServiceResourceNotFoundException(cfgFile.getAbsolutePath());
        } catch (final IOException e) {
            throw new IdentityServiceInitException(
                    String.format("An error occurs closing resource '%s'. Error skipped.", cfgFile), e);
        }
    }

    /**
     * Read a resource containing the LDAP configuration
     * 
     * @param cfgResourceName
     *            The resource name
     * @throws IdentityServiceResourceNotFoundException
     *             Resource not found
     * @throws IdentityServiceInit
     *             An error occurs reading the resource.
     */
    private LDAPConfiguration readConfigurationAsResource(final String cfgResourceName)
            throws IdentityServiceInitException {

        assert cfgResourceName != null;

        final InputStream cfgInputStream = this.getClass().getResourceAsStream(cfgResourceName);
        if (cfgInputStream != null) {
            try {
                return this.readConfiguration(cfgInputStream);
            } finally {
                try {
                    cfgInputStream.close();
                } catch (final IOException e) {
                    throw new IdentityServiceInitException(
                            String.format("An error occurs closing resource '%s'. Error skipped.", cfgResourceName), e);
                }
            }
        } else {
            throw new IdentityServiceResourceNotFoundException(cfgResourceName);
        }
    }

    private LDAPConfiguration readConfiguration(final InputStream cfgInputStream) throws IdentityServiceInitException {

        final LDAPConfiguration ldapConfiguration = new LDAPConfiguration();

        final Properties cfgProps = new Properties();
        try {
            cfgProps.load(cfgInputStream);
        } catch (final IOException e) {
            throw new IdentityServiceInitException(e);
        }
        
        //
        // Server connection parameters
        //

        final String server = cfgProps.getProperty(PROP_SERVER);
        if (server == null || server.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'server' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setServer(server);

        final String portStr = cfgProps.getProperty(PROP_PORT);
        if (portStr == null || portStr.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'port' is not defined in the identity service configuration file.");
        }
        try {
            ldapConfiguration.setPort(Integer.parseInt(portStr));
        } catch (final NumberFormatException e) {
            throw new IdentityServiceInitException("The parameter 'port' is not valid: '" + portStr + "'.");
        }
        
        //
        // Security
        //
        
        final String securityAuthenticationStr = cfgProps.getProperty(PROP_SECURITY_AUTHENTICATION);
        String securityAutehnticationValue = PROP_SECURITY_AUTHENTICATION_DEFAULT;
        if (securityAuthenticationStr == null || securityAuthenticationStr.trim().isEmpty()) {
            this.logger.config(String.format("No value set for '%s'. Default value used: '%s'.", PROP_SECURITY_AUTHENTICATION, PROP_SECURITY_AUTHENTICATION_DEFAULT));
        } else {
            if (PROP_SECURITY_AUTHENTICATION_NONE.equals(securityAuthenticationStr)
                    || PROP_SECURITY_AUTHENTICATION_SIMPLE.equals(securityAuthenticationStr)
                    || PROP_SECURITY_AUTHENTICATION_STRONG.equals(securityAuthenticationStr)) {
                securityAutehnticationValue = securityAuthenticationStr;
            } else {
                this.logger.config(String.format("Invalid value set for '%s': '%s'. Default value used: '%s'.",
                        PROP_SECURITY_AUTHENTICATION, securityAuthenticationStr, PROP_SECURITY_AUTHENTICATION_DEFAULT));
            }
        }
        ldapConfiguration.setSecurityAuthentication(securityAutehnticationValue);

        final String user = cfgProps.getProperty(PROP_USER);
        if (user != null) {
            ldapConfiguration.setUser(user);
        } else {
            this.logger.config("No user set as credentials.");
        }

        final String password = cfgProps.getProperty(PROP_PASSWORD);
        if (password != null) {
            ldapConfiguration.setPassword(password);
        } else {
            this.logger.config("No password set as credentials.");
        }

        final String searchTimeLimitStr = cfgProps.getProperty(PROP_SEARCH_TIME_LIMIT);
        if (searchTimeLimitStr != null && !searchTimeLimitStr.trim().isEmpty()) {
            try {
                ldapConfiguration.setSearchTimeLimit(Integer.parseInt(searchTimeLimitStr));
            } catch (final NumberFormatException e) {
                throw new IdentityServiceInitException(
                        "The parameter 'searchTimeLimit' is not valid: '" + searchTimeLimitStr + "'.");
            }
        } else {
            this.logger.config(String.format("No value set for '%s'. Default value used.", PROP_SEARCH_TIME_LIMIT));
        }

        //
        // Queries
        //

        final String baseDn = cfgProps.getProperty(PROP_BASE_DN);
        if (baseDn == null || baseDn.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'baseDn' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setBaseDn(baseDn);

        final String userBaseDn = cfgProps.getProperty(PROP_USER_BASE_DN);
        if (userBaseDn != null && !userBaseDn.trim().isEmpty()) {
            ldapConfiguration.setUserBaseDn(userBaseDn);
        } else {
            this.logger.config(
                    "No base distinguished name provided for user searches. Default base distinguished name (baseDn) used.");
        }

        final String groupBaseDn = cfgProps.getProperty(PROP_GROUP_BASE_DN);
        if (groupBaseDn != null && !groupBaseDn.trim().isEmpty()) {
            ldapConfiguration.setGroupBaseDn(groupBaseDn);
        } else {
            this.logger.config(
                    "No base distinguished name provided for group searches. Default base distinguished name (baseDn) used.");
        }

        //
        // Queries
        //

        final String queryUserByUserId = cfgProps.getProperty(PROP_QUERY_USER_BY_USER_ID);
        if (queryUserByUserId == null || queryUserByUserId.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'queryUserByUserId' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setQueryUserByUserId(queryUserByUserId);

        final String queryUserByFullNameLike = cfgProps.getProperty(PROP_QUERY_USER_BY_FULLNAME_LIKE);
        if (queryUserByFullNameLike == null || queryUserByFullNameLike.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'queryUserByFullNameLike' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setQueryUserByFullNameLike(queryUserByFullNameLike);

        final String queryAllUsers = cfgProps.getProperty(PROP_QUERY_ALL_USERS);
        if (queryAllUsers == null || queryAllUsers.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'queryAllUsers' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setQueryAllUsers(queryAllUsers);

        final String queryGroupsForUser = cfgProps.getProperty(PROP_QUERY_GROUPS_FOR_USER);
        if (queryGroupsForUser == null || queryGroupsForUser.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'queryGroupsForUser' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setQueryGroupsForUser(queryGroupsForUser);

        final String queryAllGroups = cfgProps.getProperty(PROP_QUERY_ALL_GROUPS);
        if (queryAllGroups == null || queryAllGroups.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'queryAllGroups' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setQueryAllGroups(queryAllGroups);

        //
        // Attributes
        //

        final String userIdAttribute = cfgProps.getProperty(PROP_ATTR_USER_ID);
        if (userIdAttribute == null || userIdAttribute.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'userIdAttribute' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setUserIdAttribute(userIdAttribute);

        final String userFirstNameAttribute = cfgProps.getProperty(PROP_ATTR_USER_FIRSTNAME);
        if (userFirstNameAttribute == null || userFirstNameAttribute.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'userFirstNameAttribute' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setUserFirstNameAttribute(userFirstNameAttribute);

        final String userLastNameAttribute = cfgProps.getProperty(PROP_ATTR_USER_LASTNAME);
        if (userLastNameAttribute == null || userLastNameAttribute.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'userLastNameAttribute' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setUserLastNameAttribute(userLastNameAttribute);

        final String userEmailAttribute = cfgProps.getProperty(PROP_ATTR_USER_EMAIL);
        if (userEmailAttribute == null || userEmailAttribute.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'userEmailAttribute' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setUserEmailAttribute(userEmailAttribute);

        final String groupIdAttribute = cfgProps.getProperty(PROP_ATTR_GROUP_ID);
        if (groupIdAttribute == null || groupIdAttribute.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'groupIdAttribute' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setGroupIdAttribute(groupIdAttribute);

        final String groupNameAttribute = cfgProps.getProperty(PROP_ATTR_GROUP_NAME);
        if (groupNameAttribute == null || groupNameAttribute.trim().isEmpty()) {
            throw new IdentityServiceInitException(
                    "The parameter 'groupNameAttribute' is not defined in the identity service configuration file.");
        }
        ldapConfiguration.setGroupNameAttribute(groupNameAttribute);

        return ldapConfiguration;
    }

}
