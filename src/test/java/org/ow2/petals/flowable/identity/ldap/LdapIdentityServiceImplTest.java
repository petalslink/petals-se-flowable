/**
 * Copyright (c) 2017-2024 Linagora
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;

import org.apache.directory.server.annotations.CreateLdapServer;
import org.apache.directory.server.annotations.CreateTransport;
import org.apache.directory.server.core.annotations.ApplyLdifFiles;
import org.apache.directory.server.core.annotations.CreateDS;
import org.apache.directory.server.core.annotations.CreatePartition;
import org.apache.directory.server.core.integ.AbstractLdapTestUnit;
import org.apache.directory.server.core.integ.ApacheDSTestExtension;
import org.flowable.engine.IdentityService;
import org.flowable.idm.api.Group;
import org.flowable.idm.api.User;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.ow2.petals.flowable.junit.extensions.FlowableClientExtension;
import org.ow2.petals.flowable.junit.extensions.FlowableClientExtension.IdmEngineType;
import org.ow2.petals.flowable.junit.extensions.api.FlowableClient;

@ExtendWith(ApacheDSTestExtension.class)
@CreateDS(partitions = { @CreatePartition(name = "Petals", suffix = LdapIdentityServiceImplTest.DOMAIN_DSN) })
@CreateLdapServer(transports = { @CreateTransport(protocol = "LDAP") })
public class LdapIdentityServiceImplTest extends AbstractLdapTestUnit {

    public static final String LDAP_USER = "cn=Petals ESB,ou=users,dc=petals,dc=org";

    public static final String DOMAIN_DSN = "dc=petals,dc=org";

    @TempDir
    private static Path tempFolder;

    @FlowableClientExtension(
            idmEngineType = IdmEngineType.LDAP, idmEngineConfiguratorCfgFile = FlowableClient.IDM_ENGINE_CONFIGURATION_TO_GENERATE, autoStart = false
    )
    private FlowableClient flowableClient;

    @BeforeEach
    private void completesFlowableClientConfigurationAndStarts() throws Exception {
        this.generateLdapIdmEngineConfigurationFile();
        this.flowableClient.start();
    }

    private void generateLdapIdmEngineConfigurationFile() throws IOException {

        // We generate a configuration from the default one to adjust the port of the LDAP server

        final Properties ldapConfig = new Properties();
        try (final InputStream defaultCfgInputStream = LdapIdentityServiceImplTest.class
                .getResourceAsStream("/ldap-idm-configurator.properties")) {
            assertNotNull(defaultCfgInputStream, "LDAP IDM configuration config file not found");
            ldapConfig.load(defaultCfgInputStream);
        }

        ldapConfig.setProperty(LdapIdmEngineConfigurator.PROP_PORT, Integer.toString(ldapServer.getPort()));
        ldapConfig.setProperty(LdapIdmEngineConfigurator.PROP_USER, LDAP_USER);
        ldapConfig.setProperty(LdapIdmEngineConfigurator.PROP_PASSWORD, "abcdefg");

        try (final OutputStream osConfigFile = new FileOutputStream(
                this.flowableClient.getIdmEngineConfigurationFile())) {
            ldapConfig.store(osConfigFile, "");
        }
    }

    /**
     * Check the loading of a given configuration.
     */
    @Test
    @ApplyLdifFiles("users-import.ldif")
    public void givenConfiguration() throws Exception {

        final IdentityService identityService = this.flowableClient.getIdentityService();

        final User kermit = identityService.createUserQuery().userId("kermit").singleResult();
        assertNotNull(kermit);
        assertEquals("Flowable", kermit.getLastName());
        assertEquals("Kermit", kermit.getFirstName());
        assertEquals("kermit@petals.org", kermit.getEmail());

        assertTrue(identityService.checkPassword("kermit", "abcdefg"));
        assertFalse(identityService.checkPassword("fozzie", "invalid-pwd"));

        final List<Group> fozzieGroups = identityService.createGroupQuery().groupMember("fozzie").list();
        assertEquals(2, fozzieGroups.size());
    }
}
